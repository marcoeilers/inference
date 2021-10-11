/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core.{AbstractLearner, Hypothesis}
import inference.util.ast.Expressions
import viper.silver.ast

/**
 * A hypothesis builder that takes a model and builds a hypothesis.
 */
trait HypothesisBuilder extends AbstractLearner {
  /**
   * Builds a hypothesis corresponding to the given templates under the given model.
   *
   * @param templates The templates.
   * @param model     The model.
   * @return The hypothesis.
   */
  protected def buildHypothesis(templates: Seq[Template], model: Map[String, Boolean]): Hypothesis = {
    // build predicates
    val predicates = templates.collect {
      case template: PredicateTemplate =>
        buildPredicate(template)(model)
    }
    // build lemmas
    val lemmas = templates.collect {
      case template: LemmaTemplate =>
        buildLemma(template)(model)
    }
    // create hypothesis
    val hypothesis = Hypothesis(predicates, lemmas)
    // return hypothesis
    logger.info(hypothesis.toString)
    hypothesis
  }

  /**
   * Builds a predicate corresponding to the given predicate template and model.
   *
   * @param template The predicate template.
   * @param model    The model.
   * @return The predicate.
   */
  private def buildPredicate(template: PredicateTemplate)(implicit model: Map[String, Boolean]): ast.Predicate = {
    // get placeholder and extract name, parameters, and atoms
    val placeholder = template.placeholder
    val name = placeholder.name
    val parameters = placeholder.parameters
    val atoms = placeholder.atoms
    // build body
    val body = buildBody(template.body, atoms)
    // create predicate
    ast.Predicate(name, parameters, body)()
  }

  /**
   * Builds a lemma method corresponding to the given lemma template and model.
   *
   * @param template The lemma template.
   * @param model    The model.
   * @return The lemma method.
   */
  private def buildLemma(template: LemmaTemplate)(implicit model: Map[String, Boolean]): ast.Method = {
    // build pre- and postcondition
    val name = template.name
    val parameters = template.parameters
    val atoms = template.atoms
    val precondition = buildSpecification(template.precondition, atoms)
    val postcondition = buildSpecification(template.postcondition, atoms)
    // create lemma method
    ast.Method(name, parameters, Seq.empty, precondition, postcondition, None)()
  }

  /**
   *
   * Builds a predicate body corresponding to the given template expression and model.
   *
   * @param expression The template expression.
   * @param atoms      The atoms upon which the predicate body may depend.
   * @param model      The model.
   * @return The predicate body.
   */
  def buildBody(expression: TemplateExpression, atoms: Seq[ast.Exp])(implicit model: Map[String, Boolean]): Option[ast.Exp] = {
    val body = buildExpression(expression, atoms)
    val simplified = Expressions.simplify(body)
    Some(simplified)
  }

  /**
   * Builds a specification corresponding to the given template expression and model.
   *
   * @param expression The template expression.
   * @param atoms      The atoms upon which the specification may depend.
   * @param model      The model.
   * @return The specification.
   */
  @inline
  def buildSpecification(expression: TemplateExpression, atoms: Seq[ast.Exp])(implicit model: Map[String, Boolean]): Seq[ast.Exp] =
    buildBody(expression, atoms).toSeq

  /**
   * Builds an expression corresponding to the given template expression and model.
   *
   * @param expression The template expression.
   * @param atoms      The atoms upon which the expression may depend.
   * @param model      The model.
   * @return The expression.
   */
  private def buildExpression(expression: TemplateExpression, atoms: Seq[ast.Exp])(implicit model: Map[String, Boolean]): ast.Exp =
    expression match {
      case Wrapped(expression) =>
        expression
      case Conjunction(conjuncts) =>
        val mapped = conjuncts.map { conjunct => buildExpression(conjunct, atoms) }
        Expressions.makeAnd(mapped)
      case Guarded(guardId, body) =>
        val guard = {
          // if the number of clauses is zero, we still want to be able to make the guard true
          val expressions =
            if (clauseCount > 0) atoms
            else Seq(ast.TrueLit()())
          buildGuard(guardId, expressions)
        }
        val guarded = buildExpression(body, atoms)
        ast.Implies(guard, guarded)()
      case Choice(choiceId, variable, options, body) =>
        // build body
        val built = buildExpression(body, atoms)
        // get choice
        val choice = getChoice(choiceId, options)
        // update body according to picked choice
        val name = variable.name
        built.transform {
          case ast.LocalVar(`name`, _) => choice
        }
      case Truncated(condition, body) =>
        val expression = buildExpression(body, atoms)
        ast.Implies(condition, expression)()
    }

  /**
   * Builds the guard with the given id corresponding to the given model.
   *
   * @param guardId The guard id.
   * @param atoms   The atoms upon which the guard may depend.
   * @param model   The model.
   * @return The guard.
   */
  private def buildGuard(guardId: Int, atoms: Seq[ast.Exp])(implicit model: Map[String, Boolean]): ast.Exp = {
    // if the number of clauses is zero, we still want to be able to make the guard true
    val count = math.max(clauseCount, 1)
    // build clauses
    val clauses = for (clauseIndex <- 0 until count) yield {
      val clauseActivation = model.getOrElse(Names.clauseActivation(guardId, clauseIndex), false)
      if (clauseActivation) {
        val literals = atoms
          .zipWithIndex
          .map { case (atom, literalIndex) => model
            .get(Names.literalActivation(guardId, clauseIndex, literalIndex))
            .flatMap { literalActivation =>
              if (literalActivation) model
                .get(Names.literalSign(guardId, clauseIndex, literalIndex))
                .map { sign => if (sign) atom else ast.Not(atom)() }
              else None
            }
            .getOrElse(ast.TrueLit()())
          }
        // conjoin literals
        Expressions.makeAnd(literals)
      } else ast.FalseLit()()
    }
    // disjoin clauses
    Expressions.makeOr(clauses)
  }

  /**
   * Returns the choice with the given choice id and options corresponding to the given model.
   *
   * @param choiceId The choice id.
   * @param options  The options.
   * @param model    The model.
   * @return The choice.
   */
  private def getChoice(choiceId: Int, options: Seq[ast.Exp])(implicit model: Map[String, Boolean]): ast.Exp =
    options.zipWithIndex
      .find { case (_, index) =>
        val name = Names.choiceActivation(choiceId, index)
        model.getOrElse(name, false)
      }
      .map { case (option, _) => option }
      .get
}
