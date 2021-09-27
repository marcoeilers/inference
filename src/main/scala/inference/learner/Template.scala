/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.Placeholder
import viper.silver.ast

/**
 * A specification template.
 */
sealed trait Template {
  /**
   * Returns the specification placeholder corresponding to this template.
   *
   * @return The specification placeholder.
   */
  def placeholder: Placeholder

  /**
   * Returns the name of the corresponding specification placeholder.
   *
   * @return The name
   */
  def name: String =
    placeholder.name

  /**
   * Returns the parameters of the specification.
   *
   * @return The parameters.
   */
  def parameters: Seq[ast.LocalVarDecl] =
    placeholder.parameters

  /**
   * Returns the atoms that may be used for the specification.
   *
   * @return The atoms.
   */
  def atoms: Seq[ast.Exp] =
    placeholder.atoms
}

/**
 * A predicate template.
 *
 * @param placeholder The specification placeholder corresponding to the template.
 * @param body        The body representing the structure allowed by the template.
 */
case class PredicateTemplate(placeholder: Placeholder, body: TemplateExpression) extends Template {
  override def toString: String =
    s"$placeholder = $body"
}

/**
 * A lemma template.
 *
 * @param placeholder   The specification placeholder corresponding to the lemma.
 * @param precondition  The template expression corresponding to the lemma precondition.
 * @param postcondition the template expression corresponding to the lemma postcondition.
 */
case class LemmaTemplate(placeholder: Placeholder, precondition: TemplateExpression, postcondition: TemplateExpression) extends Template {
  override def toString: String =
    s"$placeholder" +
      s"   requires $precondition" +
      s"   ensures $postcondition"
}

/**
 * The super trait for all template expressions.
 */
sealed trait TemplateExpression

/**
 * A template expression wrapping an expression.
 *
 * @param expression The wrapped expression.
 */
case class Wrapped(expression: ast.Exp) extends TemplateExpression {
  override def toString: String =
    expression.toString()
}

/**
 * A template expression representing a conjunction.
 *
 * @param conjuncts The conjuncts.
 */
case class Conjunction(conjuncts: Seq[TemplateExpression]) extends TemplateExpression {
  override def toString: String =
    conjuncts.mkString("(", " * ", ")")
}

/**
 * A template expression representing a guarded expression.
 *
 * @param guardId The id of the guard.
 * @param body    The guarded expression.
 */
case class Guarded(guardId: Int, body: TemplateExpression) extends TemplateExpression {
  override def toString: String =
    s"(phi_$guardId -> $body)"
}

/**
 * A template expression representing a choice for a variable.
 *
 * @param choiceId The id of the choice.
 * @param variable The variable.
 * @param options  The options.
 * @param body     The template expression for which the choice has to be made.
 */
case class Choice(choiceId: Int, variable: ast.LocalVar, options: Seq[ast.Exp], body: TemplateExpression) extends TemplateExpression {
  override def toString: String =
    s"(choose $variable from {${options.mkString(", ")} in $body)"
}

/**
 * A truncated template expression.
 *
 * @param condition The truncation condition.
 * @param body      The truncated template expression.
 */
case class Truncated(condition: ast.Exp, body: TemplateExpression) extends TemplateExpression {
  override def toString: String =
    s"($condition -> $body)"
}
