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
import inference.util.solver.Solver
import viper.silver.ast

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

    // encode choices
    map
      .flatMap { case (_, template) => collectChoices(template.body) }
      .foreach {
        case Choice(choiceId, _, options, _) =>
          // variables corresponding to options
          val variables = options
            .indices
            .map { index =>
              val name = Names.choiceActivation(choiceId, index)
              makeBoolean(name)
            }
          // encode that exactly one option per choice can be picked
          val encoding = Expressions.makeExactly(variables)
          solver.addConstraint(encoding)
      }

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
   * Collects all choices appearing in the given template expression.
   *
   * @param expression The template expression.
   * @return The choices.
   */
  private def collectChoices(expression: TemplateExpression): Seq[Choice] =
    expression match {
      case Wrapped(expression) =>
        Seq.empty
      case Conjunction(conjuncts) =>
        conjuncts.flatMap(collectChoices)
      case Guarded(_, body) =>
        collectChoices(body)
      case choice: Choice =>
        Seq(choice)
      case Truncated(_, body) =>
        collectChoices(body)
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
        val leftEncoding = encodeSample(left, !default)
        val rightEncoding = encodeSample(right, default)
        ast.Implies(leftEncoding, rightEncoding)()
      case sample@UpperBound(record) =>
        val difference = encodeDifference(record, default)
        val bound = ast.IntLit(sample.bound)()
        ast.LeCmp(difference, bound)()
    }

  /**
   * Encodes the permission differences corresponding to the given records.
   *
   * @param records   The records.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The permission difference.
   */
  private def encodeDifference(records: Seq[Record], default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    // encode permission differences
    val deltas = records.map { record => encodeDifference(record, default) }
    // return sum of differences
    Expressions.makeSum(deltas)
  }

  /**
   * Encodes the permission difference corresponding to the given record.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The permission difference.
   */
  private def encodeDifference(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): ast.Exp = {
    // encode permission differences
    val deltas = record match {
      case inhaled: InhaledRecord =>
        encodeGroupedDifferences(inhaled, default)
      case exhaled: ExhaledRecord =>
        val unsigned = encodeGroupedDifferences(exhaled, !default)
        unsigned.map { delta => ast.Minus(delta)() }
    }
    // return sum of differences
    Expressions.makeSum(deltas)
  }

  /**
   * Encodes the permission differences corresponding to the given record. Depending on the configuration, the encodings
   * are either syntactically or semantically grouped.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The grouped permission differences.
   */
  private def encodeGroupedDifferences(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): Seq[ast.Exp] = {
    val groups = encodeGroupedOptions(record, default)
    groups.map { group =>
      // implicit bounds for group
      if (configuration.useImplicitBounds) {
        val upperbound = Expressions.makeAtMost(group)
        solver.addConstraint(upperbound)
      }
      // permission difference
      val condition = Expressions.makeOr(group)
      val delta = ast.CondExp(condition, ast.IntLit(1)(), ast.IntLit(0)())()
      // return auxiliary variable holding difference
      auxiliary(delta, prefix = "d")
    }
  }

  /**
   * Encodes all options to provide permissions for the given record. Depending on the configuration, the encodings are
   * either syntactically or semantically grouped.
   *
   * @param record    The record.
   * @param default   The default value to assume for unknown predicate values (approximation).
   * @param templates The predicate templates.
   * @return The grouped options.
   */
  private def encodeGroupedOptions(record: Record, default: Boolean)(implicit templates: Map[String, PredicateTemplate]): Seq[Seq[ast.Exp]] = {
    // encode options
    val groups = Guards
      .effective(record)
      .map { case (_, effective) =>
        effective.map { guards => encodeOption(guards, record.state, default) }
      }
      .toSeq
    // flatten syntactic group into one semantic group if semantic bounds are enabled
    if (configuration.useSemanticBounds) Seq(groups.flatten)
    else groups
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
      case ChoiceGuard(choiceId, index) =>
        encodeChoice(choiceId, index)
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
      val clauseActivation = {
        val name = Names.clauseActivation(guardId, clauseIndex)
        makeBoolean(name)
      }
      val clauseEncoding = {
        // encode literals
        val literals = values
          .zipWithIndex
          .map { case (value, literalIndex) =>
            val literalActivation = {
              val name = Names.literalActivation(guardId, clauseIndex, literalIndex)
              makeBoolean(name)
            }
            val literalEncoding = value match {
              case Some(sign) =>
                val name = Names.literalSign(guardId, clauseIndex, literalIndex)
                val positive = makeBoolean(name)
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
   * Encodes the given choice.
   *
   * @param choiceId The choice id.
   * @param index    The index of the choice.
   * @return The encoding.
   */
  private def encodeChoice(choiceId: Int, index: Int): ast.Exp = {
    val name = Names.choiceActivation(choiceId, index)
    makeBoolean(name)
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
