/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.Hypothesis
import inference.util.ast.Expressions
import viper.silver.ast

/**
 * A hypothesis builder.
 */
trait HypothesisBuilder {
  /**
   * Builds a hypothesis corresponding to the given templates under the given model.
   *
   * @param templates The templates.
   * @param model     The model.
   * @return The hypothesis.
   */
  def buildHypothesis(templates: Seq[Template], model: Map[String, Boolean]): Hypothesis = {
    // build predicates
    val predicates = templates.map {
      case template: PredicateTemplate => buildPredicate(template, model)
    }
    // create hypothesis
    Hypothesis(predicates)
  }

  /**
   * Builds a predicate corresponding to the given template under the given model.
   *
   * @param template The predicate template.
   * @param model    The model.
   * @return The predicate.
   */
  private def buildPredicate(template: PredicateTemplate, model: Map[String, Boolean]): ast.Predicate = {
    // get placeholder and extract name and parameters
    val placeholder = template.placeholder
    val name = placeholder.name
    val parameters = placeholder.parameters
    // build body
    val body = buildExpression(template.body, model)
    // create predicate
    ast.Predicate(name, parameters, Some(body))()
  }

  /**
   * Builds an expression corresponding to the given template expression under the given model.
   *
   * @param expression The template expression.
   * @param model      The model.
   * @return The expression.
   */
  private def buildExpression(expression: TemplateExpression, model: Map[String, Boolean]): ast.Exp =
    expression match {
      case Wrapped(expression) =>
        expression
      case Conjunction(conjuncts) =>
        val mapped = conjuncts.map { conjunct => buildExpression(conjunct, model) }
        Expressions.conjoin(mapped)
      case Guarded(guardId, body) =>
        // TODO: Build guard.
        buildExpression(body, model)
    }
}
