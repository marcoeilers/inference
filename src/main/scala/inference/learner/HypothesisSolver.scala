/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core.sample._
import inference.input.{Configuration, Input}
import inference.util.Namespace
import inference.util.ast.Expressions
import inference.util.collections.{Collections, SeqMap}
import inference.util.solver.Solver
import viper.silver.ast

import java.util.concurrent.atomic.AtomicInteger

/**
 * A hypothesis solver that encodes samples and (hopefully) returns a suitable model.
 */
trait HypothesisSolver {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

  /**
   * The solver.
   */
  protected val solver: Solver

  /**
   * Returns the samples.
   *
   * @return The samples
   */
  protected def samples: Seq[Sample]

  /**
   * The namespace used to generate unique names.
   */
  var namespace: Namespace = _

  /**
   * Encodes all accumulated samples under consideration of the given templates and returns a suitable model found by
   * the solver.
   *
   * @param templates The templates.
   * @return The model.
   */
  def solve(templates: Seq[Template]): Option[Map[String, Boolean]] = {
    // reset namespace
    namespace = new Namespace(template = "%s-%d")

    // create mapping from names to predicate templates
    implicit val map: Map[String, PredicateTemplate] =
      templates
        .collect { case template: PredicateTemplate => template.name -> template }
        .toMap

    // encode samples
    samples.foreach { sample =>
      solver.addComment(sample.toString)
      val encoding = encodeSample(sample, default = false)
      solver.addConstraint(encoding)
    }

    // solve constraints and return model
    solver.solve()
  }

  /**
   * Encodes the given sample under consideration of the implicitly passed predicate templates.
   *
   * @param sample    The samples.
   * @param default   The default value to assume for unknown atomic predicate values (approximation).
   * @param templates The predicate templates.
   * @return The encoding.
   */
  private def encodeSample(sample: Sample, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp =
    sample match {
      case sample@LowerBound(records) =>
        val difference = encodeDifference(records, default)
        val bound = ast.IntLit(sample.bound)()
        ast.GtCmp(difference, bound)()
      case Implication(left, right) =>
        val leftEncoding = encodeAtLeast(left, !default)
        val rightEncoding = encodeSample(right, default)
        ast.Implies(leftEncoding, rightEncoding)()
      case UpperBound(record) =>
        encodeAtMost(record, default)
    }

  /**
   * Encodes the permission difference corresponding to the given records under consideration of the implicitly passed
   * predicate templates.
   *
   * @param records   The records.
   * @param default   The default value to assume for unknown atomic predicate values (approximation).
   * @param templates The predicate templates
   * @return
   */
  private def encodeDifference(records: Seq[Record], default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    val differences = records.map { record => encodeDifference(record, default) }
    Expressions.makeSum(differences)
  }

  /**
   * Encodes the permission difference corresponding to the given record under consideration of the implicitly passed
   * predicate templates.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown atomic predicate values (approximation).
   * @param templates The predicate templates.
   * @return The encoding.
   */
  private def encodeDifference(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    // permission difference
    val delta = record match {
      case inhaledRecord: InhaledRecord =>
        val condition = encodeAtLeast(inhaledRecord, default)
        ast.CondExp(condition, ast.IntLit(1)(), ast.IntLit(0)())()
      case exhaledRecord: ExhaledRecord =>
        val condition = encodeAtLeast(exhaledRecord, !default)
        ast.CondExp(condition, ast.IntLit(-1)(), ast.IntLit(0)())()
    }
    // return auxiliary variable holding difference
    auxiliary(delta, prefix = "d")
  }

  /**
   * Encodes that at least one of the options to provide permissions for the given record should be picked.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The encoding.
   */
  private def encodeAtLeast(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    val options = encodeOptions(record, default)
    Expressions.makeOr(options)
  }

  /**
   * Encodes that at most one of the options to provide permissions for the given record should be picked.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The encoding.
   */
  private def encodeAtMost(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    val options = encodeOptions(record, default)
    val constraints = Collections
      .pairs(options)
      .map { case (left, right) => ast.Not(ast.And(left, right)())() }
    Expressions.makeAnd(constraints)
  }

  /**
   * Encodes all options to provide permissions for the given record.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The encoding.
   */
  private def encodeOptions(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): Seq[ast.Exp] = {
    val state = record.state
    Guards
      .effective(record)
      .flatMap { case (_, effective) =>
        effective.map { guards => encodeOption(guards, state, default) }
      }
      .toSeq
  }

  /**
   * Encodes an option represented by the given guards.
   *
   * @param guards  The guards.
   * @param state   The state.
   * @param default The default value to assume for unknown predicate values (approximation).
   * @return The encoding.
   */
  private def encodeOption(guards: Seq[Guard], state: StateAbstraction, default: Boolean): ast.Exp = {
    // create conjuncts corresponding to guards
    val conjuncts = guards.map {
      case ResourceGuard(guardId, atoms) =>
        val values = state.evaluate(atoms)
        encodeState(guardId, values, default)
      case ChoiceGuard(_, _) =>
        // TODO: Implement me.
        ???
      case TruncationGuard(condition) =>
        state
          .evaluate(condition)
          .map { value => ast.BoolLit(value)() }
          .get
    }
    // return auxiliary variable constrained to the encoding
    val encoding = Expressions.makeAnd(conjuncts)
    auxiliary(encoding)
  }

  /**
   * Encodes the state represented by the given values for the guard with the given id.
   *
   * @param guardId The guard id.
   * @param values  The values representing the state.
   * @param default The default value to assume for unknown predicate values (approximation).
   * @return The encoding.
   */
  private def encodeState(guardId: Int, values: Seq[Option[Boolean]], default: Boolean): ast.Exp = {
    // encode clauses
    val clauses = for (clauseIndex <- 0 until configuration.maxClauses) yield {
      val clauseActivation = makeBoolean(Names.clauseActivation(guardId, clauseIndex))
      val clauseEncoding = {
        // encode literals
        val literals = values
          .zipWithIndex
          .map { case (value, literalIndex) =>
            val literalActivation = makeBoolean(Names.literalActivation(guardId, clauseIndex, literalIndex))
            val literalEncoding = value match {
              case Some(sign) =>
                val positive = makeBoolean(Names.literalSign(guardId, clauseIndex, literalIndex))
                if (sign) positive else ast.Not(positive)()
              case None =>
                ast.BoolLit(default)()
            }
            ast.Implies(literalActivation, literalEncoding)()
          }
        // conjoin literals
        Expressions.makeAnd(literals)
      }
      ast.And(clauseActivation, clauseEncoding)()
    }
    // disjoin clauses
    Expressions.makeOr(clauses)
  }

  /**
   * Returns an auxiliary variable constrained to be equal to the given expression.
   *
   * @param expression The expression.
   * @param prefix     The prefix to use for the variable name.
   * @return The auxiliary variable.
   */
  private def auxiliary(expression: ast.Exp, prefix: String = "t"): ast.Exp = {
    // create variable
    val name = namespace.uniqueIdentifier(prefix)
    val variable = makeVariable(name, expression.typ)
    // constrain variable
    val constraint = ast.EqCmp(variable, expression)()
    solver.addConstraint(constraint)
    // return variable
    variable
  }

  /**
   * Creates a boolean-typed variable with the given name.
   *
   * @param name The name of the variable.
   * @return The variable.
   */
  @inline
  private def makeBoolean(name: String): ast.Exp =
    makeVariable(name, ast.Bool)

  /**
   * Creates a variable with the given name and type.
   *
   * @param name The name of the variable.
   * @param typ  The type of the variable.
   * @return The variable.
   */
  @inline
  private def makeVariable(name: String, typ: ast.Type): ast.Exp =
    ast.LocalVar(name, typ)()
}
