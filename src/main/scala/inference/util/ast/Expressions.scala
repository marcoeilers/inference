/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.ast

import viper.silver.ast
import viper.silver.ast.utility.rewriter.Traverse

object Expressions {
  /**
   * Returns the conjunction of the given expressions.
   *
   * @param expressions The expressions to conjoin.
   * @return The conjunction.
   */
  def conjoin(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.And(_, _)())
      .getOrElse(ast.TrueLit()())

  /**
   * Returns the disjunction of the given expressions.
   *
   * @param expressions The expressions to disjoin.
   * @return The disjunction.
   */
  def disjoin(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.Or(_, _)())
      .getOrElse(ast.FalseLit()())

  /**
   * Returns the sum of the given expressions.
   *
   * @param expressions The expressions to add up.
   * @return The sum.
   */
  def sum(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.Add(_, _)())
      .getOrElse(ast.IntLit(0)())

  /**
   * Simplifies the given expression.
   *
   * @param expression The expression to simplify.
   * @return The simplified expression.
   */
  def simplify(expression: ast.Exp): ast.Exp =
    expression.transform(term => simplification(term), Traverse.BottomUp)

  /**
   * Performs a simplification step.
   *
   * @param expression The expression to simplify.
   * @return The simplified expression.
   */
  private def simplification(expression: ast.Node): ast.Node =
    expression match {
      // simplify equality
      case ast.EqCmp(left, right) =>
        if (left == right) ast.TrueLit()()
        else expression
      // simplify inequality
      case ast.NeCmp(left, right) =>
        if (left == right) ast.FalseLit()()
        else expression
      // simplify negations
      case ast.Not(argument) => argument match {
        case ast.TrueLit() => ast.FalseLit()()
        case ast.FalseLit() => ast.TrueLit()()
        case ast.Not(nested) => nested
        case ast.EqCmp(left, right) => ast.NeCmp(left, right)()
        case ast.NeCmp(left, right) => ast.EqCmp(left, right)()
        case _ => expression
      }
      // simplify conjunction
      case ast.And(left, right) => (left, right) match {
        case (ast.TrueLit(), _) => right
        case (_, ast.TrueLit()) => left
        case (ast.FalseLit(), _) => ast.FalseLit()()
        case (_, ast.FalseLit()) => ast.FalseLit()()
        case _ => expression
      }
      // simplify disjunction
      case ast.Or(left, right) => (left, right) match {
        case (ast.TrueLit(), _) => ast.TrueLit()()
        case (_, ast.TrueLit()) => ast.TrueLit()()
        case (ast.FalseLit(), _) => right
        case (_, ast.FalseLit()) => left
        case _ => expression
      }
      // simplify implication
      case ast.Implies(left, right) => (left, right) match {
        case (ast.TrueLit(), _) => right
        case (ast.FalseLit(), _) => ast.TrueLit()()
        case _ => expression
      }
      // do nothing by default
      case _ => expression
    }
}
