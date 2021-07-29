/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core.{ExhaledRecord, Implication, InhaledRecord, LowerBound, Record, Sample, UpperBound}
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

  private val id: AtomicInteger =
    new AtomicInteger()

  /**
   * Computes guard maps for the given templates.
   *
   * @param templates The templates.
   * @return The guard maps.
   */
  private def computeGuardMaps(templates: Seq[Template]): Map[String, GuardMap] =
    templates
      .flatMap {
        case template: PredicateTemplate =>
          val name = template.name
          val map = computeGuardMap(template)
          Some(name -> map)
        case _ =>
          None
      }
      .toMap

  /**
   * Computes the guard map for the given template.
   *
   * @param template The template.
   * @return The guard map.
   */
  private def computeGuardMap(template: PredicateTemplate): GuardMap = {
    val buffer: mutable.Buffer[(ast.LocationAccess, Seq[Guard])] = ListBuffer.empty
    val atoms = template.placeholder.atoms

    /**
     * Helper method that processes the given template expression.
     *
     * @param expression The template expression.
     * @param guards     The guards guarding the template expression.
     */
    def process(expression: TemplateExpression, guards: Seq[Guard]): Unit =
      expression match {
        case Wrapped(wrapped) =>
          wrapped match {
            case ast.FieldAccessPredicate(location, _) =>
              buffer.append(location -> guards)
            case _ =>
              sys.error(s"Unexpected wrapped expression in template: $wrapped")
          }
        case Conjunction(conjuncts) =>
          conjuncts.foreach { conjunct => process(conjunct, guards) }
        case Guarded(guardId, body) =>
          val resourceGuard = ResourceGuard(guardId, atoms)
          process(body, guards :+ resourceGuard)
      }

    // process template body
    process(template.body, Seq.empty)
    // build guard map
    buffer.foldLeft(Map.empty: GuardMap) {
      case (map, (location, guards)) =>
        SeqMap.add(map, location, guards)
    }
  }

  /**
   * Encodes the given samples under consideration of the given templates and returns a suitable model.
   *
   * @param templates The templates.
   * @param samples   The samples to encode.
   * @return The model.
   */
  def solve(templates: Seq[Template], samples: Seq[Sample]): Map[String, Boolean] = {
    // clear counters
    id.set(0)
    // compute effective guards for templates
    val guardMaps = computeGuardMaps(templates)
    // encode samples
    samples.foreach { sample =>
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
      case LowerBound(records, bound) =>
        val difference = encodeDifference(records, guardMaps)
        ast.GeCmp(difference, ast.IntLit(bound)())()
      case Implication(left, right) =>
        val encodedLeft = encodeAtLeast(left, guardMaps, default = true)
        val encodedRight = encodeSample(right, guardMaps)
        ast.Implies(encodedLeft, encodedRight)()
      case UpperBound(record, bound) =>
        assert(bound == 1)
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
    Expressions.sum(deltas)
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
    Expressions.disjoin(options)
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
    Expressions.conjoin(constraints)
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
    val guardMap = guardMaps(name)
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
          Expressions.conjoin(conjuncts)
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
        Expressions.conjoin(literals)
      }
      ast.And(clauseActivation, clauseEncoding)()
    }
    // disjoin clauses
    Expressions.disjoin(clauses)
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
}
