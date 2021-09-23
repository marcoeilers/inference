/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Check, Hint, Input}
import inference.util.ast.Statements
import viper.silver.ast

/**
 * A mixin providing methods to emit ghost code.
 */
trait GhostCode extends Builder with Simplification {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Returns the check currently being processed.
   *
   * @return The current check.
   */
  protected def check: Check

  /**
   * Unfolds the given expression.
   *
   * @param expression The expression to unfold.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The current hypothesis.
   * @param default    The default action for leaf expressions.
   */
  protected def unfold(expression: ast.Exp,
                       simplify: Boolean = false)
                      (implicit hypothesis: Hypothesis,
                       default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    if (simplify) simplified(unfold(expression))
    else {
      // TODO: Depth
      val depth = 1
      recursiveUnfold(expression, depth)
    }

  /**
   * Folds the given expression.
   *
   * @param expression The expression to fold.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The current hypothesis.
   */
  protected def fold(expression: ast.Exp, simplify: Boolean = false)
                    (implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit =
    if (simplify) simplified(fold(expression))
    else {
      // TODO: Depth
      val depth = 2
      implicit val exhale: Boolean = false
      implicit val info: ast.Info = ast.NoInfo
      recursiveFold(expression, depth)
    }

  /**
   * Exhales the given expression.
   *
   * @param expression The expression to exhale.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The current hypothesis.
   * @param hints      The hints.
   * @param info       The info to attach to exhaled resources.
   */
  protected def exhale(expression: ast.Exp, simplify: Boolean = false)
                      (implicit hypothesis: Hypothesis, hints: Seq[Hint], info: ast.Info): Unit =
    if (simplify) simplified(exhale(expression))
    else {
      // TODO: Depth
      val depth = 2
      implicit val exhale: Boolean = true
      recursiveFold(expression, depth)
    }

  /**
   * Recursively unfolds the given expression up to the given depth.
   *
   * @param expression The expression to unfold.
   * @param depth      The current depth.
   * @param outer      The collected guards already handled by a conditional statement.
   * @param guards     The collected guards not yet handled by a conditional statement.
   * @param hypothesis The current hypothesis.
   * @param default    The default action for leaf expressions.
   */
  private def recursiveUnfold(expression: ast.Exp,
                              depth: Int,
                              outer: Seq[ast.Exp] = Seq.empty,
                              guards: Seq[ast.Exp] = Seq.empty)
                             (implicit hypothesis: Hypothesis,
                              default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        recursiveUnfold(left, depth, outer, guards)
        recursiveUnfold(right, depth, outer, guards)
      case ast.Implies(left, right) =>
        val updatedGuards = guards :+ left
        recursiveUnfold(right, depth, outer, updatedGuards)
      case resource@ast.PredicateAccessPredicate(predicate, _) if depth > 0 =>
        // create unfolds
        val unfolds = makeScope {
          // unfold predicate
          emitUnfold(resource)
          // recursively unfold predicates appearing in body
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          recursiveUnfold(body, depth - 1, outer ++ guards)
        }
        // conditionally unfold
        emitConditional(guards, unfolds)
      case other =>
        default(other, outer ++ guards)
    }

  /**
   * Recursively folds (or exhales) the given expression up to the given depth. Moreover, it uses the given hints to
   * potentially apply some lemmas.
   *
   * @param expression The expression to fold.
   * @param hints      The hints.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param exhale     The flag indicating whether the expression should be exhaled instead of folded.
   * @param info       The info to attach to the fold (or exhale) statements.
   */
  private def foldWithHints(expression: ast.Exp, hints: Seq[Hint], depth: Int)
                           (implicit hypothesis: Hypothesis, exhale: Boolean, info: ast.Info): Unit = {
    // TODO: Process hints
    recursiveFold(expression, depth)
  }

  /**
   * Recursively folds (or exhales) the given expression up to the given depth.
   *
   * @param expression The expression to fold .
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param exhale     The flag indicating whether the expression should be exhaled instead of folded.
   * @param info       The info to attach to the fold (or exhale) statements.
   */
  private def recursiveFold(expression: ast.Exp,
                            depth: Int)
                           (implicit hypothesis: Hypothesis, exhale: Boolean, info: ast.Info): Unit =
    expression match {
      case ast.And(left, right) =>
        recursiveFold(right, depth)
        recursiveFold(left, depth)
      case ast.Implies(left, right) =>
        val inner = makeScope(recursiveFold(right, depth))
        emitConditional(left, inner)
      case resource@ast.PredicateAccessPredicate(predicate, _) if depth > 0 =>
        // ghost code
        val code = {
          // action to perform if the predicate instance is present
          val thenBody = makeScope {
            // recursively process instances appearing in predicate body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            recursiveFold(body, depth - 1)
            // fold predicate (unless we exhale everything)
            if (!exhale) emitFold(resource, info)
          }
          // action to perform if the predicate instance is not present
          val b = makeScope {
            if (exhale) emitExhale(resource, info)
          }
          // create conditional statement
          val condition = noPermission(predicate)
          Statements.makeConditional(condition, thenBody, b)
        }
        // only exhale predicate if it is framed. if it is not there will be a subsequent exhale of the missing
        // permission that we want to fail (since specifications are well-formed)
        if (exhale) {
          val framed = predicate
            .args
            .collect { case field: ast.FieldAccess => somePermission(field) }
          emitConditional(framed, code)
        } else emit(code)
      case other =>
        if (exhale) emitExhale(other, info)
    }

  /**
   * Returns an expression representing the condition that there is no permission for the given location access.
   *
   * @param access The location access.
   * @return The expression representing the condition.
   */
  private def noPermission(access: ast.ResourceAccess): ast.Exp = {
    val current = ast.CurrentPerm(access)()
    val write = ast.FullPerm()()
    ast.PermLtCmp(current, write)()
  }

  /**
   * Returns an expression representing the condition that there is some permission for the given location access.
   *
   * @param access The location access.
   * @return The expression representing the condition.
   */
  private def somePermission(access: ast.ResourceAccess): ast.Exp = {
    val current = ast.CurrentPerm(access)()
    val write = ast.NoPerm()()
    ast.PermGtCmp(current, write)()
  }
}
