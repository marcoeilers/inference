/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core._
import inference.util.ast.Expressions
import inference.util.collections.{Collections, SeqMap}
import inference.util.solver.Solver
import viper.silver.ast

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A hypothesis solver that encodes samples and (hopefully) returns a suitable model.
 */
trait HypothesisSolver {
  private type GuardMap = Map[ast.LocationAccess, Seq[Seq[Guard]]]
  /**
   * The solver.
   */
  protected val solver: Solver

  /**
   * Returns the samples.
   *
   * @return The samples.
   */
  protected def samples: Seq[Sample]

  /**
   * The id used to generate unique names.
   */
  private val id: AtomicInteger =
    new AtomicInteger()

  /**
   * Computes guard maps for the given templates.
   *
   * @param templates The templates.
   * @return The guard maps.
   */
  private def computeGuardMaps(templates: Seq[Template]): Map[String, GuardMap] = {
    // collect all predicate templates
    val predicates =
      templates
        .flatMap {
          case template: PredicateTemplate => Some(template.name -> template)
          case _ => None
        }
        .toMap

    // buffer used to accumulate options
    val options: mutable.Buffer[(ast.LocationAccess, Seq[Guard])] = ListBuffer.empty

    /**
     * Processes the given template.
     *
     * @param template The template to process.
     * @param depth    The current depth.
     * @param view     The current view.
     * @param guards   The current guards.
     */
    def processTemplate(template: PredicateTemplate, depth: Int, view: View = View.empty, guards: Seq[Guard] = Seq.empty): Unit =
      if (depth > 0) {
        // get and adapt atoms
        val atoms = template
          .placeholder
          .atoms
          .map(view.adapt)
        // process template body
        val body = template.body
        processExpression(body, view, guards)(depth, atoms)
      }

    /**
     * Helper method that processes the given template expression.
     *
     * @param expression The template expression to process
     * @param view       The current view.
     * @param guards     The current guards.
     * @param depth      The implicitly passed depth.
     * @param atoms      The implicitly passed atoms.
     */
    def processExpression(expression: TemplateExpression, view: View, guards: Seq[Guard])
                         (implicit depth: Int, atoms: Seq[ast.Exp]): Unit =
      expression match {
        case Wrapped(wrapped) =>
          wrapped match {
            case ast.FieldAccessPredicate(ast.FieldAccess(receiver, field), _) =>
              // add guards for field access
              val adapted = view.adapt(receiver)
              val access = ast.FieldAccess(adapted, field)()
              options.append(access -> guards)
            case ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _) =>
              // add guards for predicate access
              val adapted = arguments.map(view.adapt)
              val access = ast.PredicateAccess(adapted, name)()
              options.append(access -> guards)
              // recursively process template
              val innerTemplate = predicates(name)
              val innerView = View.create(innerTemplate, adapted)
              processTemplate(innerTemplate, depth - 1, innerView, guards)
            case _ =>
              sys.error(s"Unexpected wrapped expression in template: $wrapped")
          }
        case Conjunction(conjuncts) =>
          conjuncts.foreach { conjunct => processExpression(conjunct, view, guards) }
        case Guarded(guardId, body) =>
          val resourceGuard = ResourceGuard(guardId, atoms)
          processExpression(body, view, guards :+ resourceGuard)
      }

    // compute effective guards for all predicate templates
    templates
      .flatMap {
        case template: PredicateTemplate =>
          // process template
          options.clear()
          processTemplate(template, depth = 3)
          // build guard map
          val map = options.foldLeft(Map.empty: GuardMap) {
            case (map, (location, guards)) =>
              SeqMap.add(map, location, guards)
          }
          // add map
          val name = template.name
          Some(name -> map)
        case _ =>
          None
      }
      .toMap
  }

  /**
   * Encodes the currently accumulated samples under consideration of the given templates and returns a suitable model.
   *
   * @param templates The templates.
   * @return The model.
   */
  def solve(templates: Seq[Template]): Option[Map[String, Boolean]] = {
    // clear counters
    id.set(0)

    // compute effective guards for templates
    val guardMaps = computeGuardMaps(templates)
    // encode samples
    samples.foreach { sample =>
      solver.addComment(sample.toString)
      val encoding = encodeSample(sample, guardMaps)
      solver.addConstraint(encoding)
    }
    // solve constraints and return model
    solver.solve()
  }

  /**
   * Encodes the given sample under consideration of the given effective guards.
   *
   * @param sample    The sample to encode.
   * @param guardMaps The effective guards.
   * @return The encoding.
   */
  private def encodeSample(sample: Sample, guardMaps: Map[String, GuardMap]): ast.Exp =
    sample match {
      case sample@LowerBound(records) =>
        val difference = encodeDifference(records, guardMaps)
        val bound = ast.IntLit(sample.bound)()
        ast.GtCmp(difference, bound)()
      case Implication(left, right) =>
        val encodedLeft = encodeAtLeast(left, guardMaps, default = true)
        val encodedRight = encodeSample(right, guardMaps)
        ast.Implies(encodedLeft, encodedRight)()
      case UpperBound(record) =>
        encodeAtMost(record, guardMaps, default = true)
    }

  /**
   * Encodes the under-approximate permission difference corresponding to the given records.
   *
   * @param records   The records.
   * @param guardMaps The effective guards.
   * @return The permission difference.
   */
  private def encodeDifference(records: Seq[Record], guardMaps: Map[String, GuardMap]): ast.Exp = {
    val deltas = records
      .map { record =>
        // auxiliary variable
        val variable = {
          val name = s"d-${id.getAndIncrement()}"
          ast.LocalVar(name, ast.Int)()
        }
        // permission difference
        val delta = record match {
          case inhaledRecord: InhaledRecord =>
            val condition = encodeAtLeast(inhaledRecord, guardMaps, default = false)
            ast.CondExp(condition, ast.IntLit(1)(), ast.IntLit(0)())()
          case exhaledRecord: ExhaledRecord =>
            val condition = encodeAtLeast(exhaledRecord, guardMaps, default = true)
            ast.CondExp(condition, ast.IntLit(-1)(), ast.IntLit(0)())()
        }
        // constrain variable to difference
        val constraint = ast.EqCmp(variable, delta)()
        solver.addConstraint(constraint)
        // map function returns variable holding difference
        variable: ast.Exp
      }
    Expressions.bigSum(deltas)
  }

  /**
   * Encodes that at least one of the options providing permissions for the given record should be picked.
   *
   * @param record    The record.
   * @param guardMaps The effective guards.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @return The encoding.
   */
  private def encodeAtLeast(record: Record, guardMaps: Map[String, GuardMap], default: Boolean): ast.Exp = {
    val options = encodeOptions(record, guardMaps, default)
    Expressions.bigOr(options)
  }

  /**
   * Encodes that at most one of the options providing permissions for the given record should be picked.
   *
   * @param record    The record.
   * @param guardMaps The effective guards.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @return The encoding.
   */
  private def encodeAtMost(record: Record, guardMaps: Map[String, GuardMap], default: Boolean): ast.Exp = {
    val options = encodeOptions(record, guardMaps, default)
    val constraints = Collections
      .pairs(options)
      .map { case (first, second) => ast.Not(ast.And(first, second)())() }
    Expressions.bigAnd(constraints)
  }

  /**
   * Encodes all options corresponding to the given record.
   *
   * @param record    The record.
   * @param guardMaps The effective guards.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @return The encoding.
   */
  private def encodeOptions(record: Record, guardMaps: Map[String, GuardMap], default: Boolean): Iterable[ast.Exp] = {
    // get guard map and state abstraction
    val name = record.placeholder.name
    val guardMap = guardMaps.getOrElse(name, Map.empty)
    val abstraction = record.abstraction
    // encode options
    record
      .locations
      .flatMap { location =>
        val guards = guardMap(location)
        guards.map { sequence =>
          val conjuncts = sequence
            .map {
              case ResourceGuard(guardId, atoms) =>
                val values = abstraction.evaluate(atoms)
                encodeState(guardId, values, default)
              case ChoiceGuard(_, _) =>
                // TODO: Implement me.
                ???
              case TruncationGuard(_) =>
                // TODO: Implement me.
                ???
            }
          Expressions.bigAnd(conjuncts)
        }
      }
  }

  /**
   * Encodes the state defined by the given values for the guard with the given id.
   *
   * @param guardId The guard id.
   * @param values  The values defining the state.
   * @param default The default value to assume for unknown predicate values (approximation).
   * @return THe encoding.
   */
  private def encodeState(guardId: Int, values: Seq[Option[Boolean]], default: Boolean): ast.Exp = {
    val maxClauses = 1
    // encode clauses
    val clauses = for (clauseIndex <- 0 until maxClauses) yield {
      val clauseActivation = variable(Names.clauseActivation(guardId, clauseIndex))
      val clauseEncoding = {
        // encode literals
        val literals = values
          .zipWithIndex
          .map { case (value, literalIndex) =>
            val literalActivation = variable(Names.literalActivation(guardId, clauseIndex, literalIndex))
            val literalEncoding = value match {
              case Some(sign) =>
                val positive = variable(Names.literalSign(guardId, clauseIndex, literalIndex))
                if (sign) positive else ast.Not(positive)()
              case None =>
                ast.BoolLit(default)()
            }
            ast.Implies(literalActivation, literalEncoding)()
          }
        // conjoin literals
        Expressions.bigAnd(literals)
      }
      ast.And(clauseActivation, clauseEncoding)()
    }
    // disjoin clauses
    Expressions.bigOr(clauses)
  }

  @inline
  private def variable(name: String): ast.Exp =
    ast.LocalVar(name, ast.Bool)()

  /**
   * The super trait for all guards (used to represent effective guards).
   */
  sealed trait Guard

  /**
   * A resource guard.
   *
   * @param guardId The guard id.
   * @param atoms   The atomic predicates.
   */
  case class ResourceGuard(guardId: Int, atoms: Seq[ast.Exp]) extends Guard {
    override def toString: String =
      s"phi_$guardId[${atoms.mkString(", ")}]"
  }

  /**
   * A choice guard.
   *
   * @param choiceId The choice id.
   * @param index    The index of the selected choice.
   */
  case class ChoiceGuard(choiceId: Int, index: Int) extends Guard {
    override def toString: String =
      s"c_$choiceId=$index"
  }

  /**
   * A truncation guard.
   *
   * @param condition The truncation condition.
   */
  case class TruncationGuard(condition: ast.Exp) extends Guard {
    override def toString: String =
      condition.toString()
  }

  /**
   * The companion object for views.
   */
  object View {
    /**
     * Returns the empty view that does not associated any variable with any expression.
     *
     * @return The empty view.
     */
    def empty: View =
      View(Map.empty)

    /**
     * Creates the view for the given template with the given arguments.
     *
     * @param template  The template.
     * @param arguments The arguments.
     * @return The view.
     */
    def create(template: Template, arguments: Seq[ast.Exp]): View = {
      val map = template
        .placeholder
        .parameters
        .map(_.name)
        .zip(arguments)
        .toMap
      View(map)
    }
  }

  /**
   * A view mapping variable names ot the expressions with which they are instantiated.
   *
   * @param map The map.
   */
  case class View(map: Map[String, ast.Exp]) {
    /**
     * Returns whether the view is empty.
     *
     * @return True if the view is empty.
     */
    def isEmpty: Boolean =
      map.isEmpty

    /**
     * Adapts the expression according to the view.
     *
     * @param expression The expression to adapt.
     * @return The adapted expression.
     */
    def adapt(expression: ast.Exp): ast.Exp =
      if (isEmpty) expression
      else expression.transform {
        case variable@ast.LocalVar(name, _) =>
          map.getOrElse(name, variable)
      }

    /**
     * Returns the updated view where the variable with the given name is associated with the given expression.
     *
     * @param name       The name of the variable.
     * @param expression The expression.
     * @return The updated view.
     */
    def updated(name: String, expression: ast.Exp): View =
      View(map.updated(name, expression))
  }
}
