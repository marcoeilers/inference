/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Check, Input}
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
                    (implicit hypothesis: Hypothesis): Unit =
    if (simplify) simplified(fold(expression))
    else {
      // TODO: Depth
      val depth = 2
      recursiveFold(expression, depth)
    }

  /**
   * Exhales the given expression.
   *
   * @param expression The expression to exhale.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to exhaled resources.
   */
  protected def exhale(expression: ast.Exp, simplify: Boolean = false)
                      (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    if (simplify) simplified(exhale(expression))
    else {
      // TODO: Depth
      val depth = 2
      adaptiveExhale(expression, depth)
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
   * Recursively folds the given expression up to the given depth.
   *
   * @param expression The expression to fold.
   * @param depth      The current depth.
   * @param guards     The collected guards.
   * @param hypothesis The current hypothesis.
   */
  private def recursiveFold(expression: ast.Exp, depth: Int, guards: Seq[ast.Exp] = Seq.empty)
                           (implicit hypothesis: Hypothesis): Unit =
    expression match {
      case ast.And(left, right) =>
        recursiveFold(right, depth, guards)
        recursiveFold(left, depth, guards)
      case ast.Implies(left, right) =>
        val updatedGuards = guards :+ left
        recursiveFold(right, depth, updatedGuards)
      case resource@ast.PredicateAccessPredicate(predicate, _) if depth > 0 =>
        // create folds
        val folds = makeScope {
          // recursively fold predicates appearing in body
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          recursiveFold(body, depth - 1)
          // fold predicate
          emitFold(resource)
        }
        // conditionally fold
        val condition = noPermission(predicate)
        emitConditional(guards :+ condition, folds)
      case _ => // do nothing
    }

  /**
   * Adaptively exhales the given expression up to the given depth.
   *
   * @param expression The expression to exhale.
   * @param depth      The current depth.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to exhaled resources.
   */
  private def adaptiveExhale(expression: ast.Exp, depth: Int)
                            (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    expression match {
      case ast.And(left, right) =>
        adaptiveExhale(right, depth)
        adaptiveExhale(left, depth)
      case ast.Implies(left, right) =>
        val exhales = makeScope(adaptiveExhale(right, depth))
        emitConditional(left, exhales)
      case resource@ast.PredicateAccessPredicate(predicate, _) if depth > 0 =>
        // recursive exhales
        val recursive = makeScope {
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          adaptiveExhale(body, depth - 1)
        }
        // direct exhale
        val direct = makeScope(emitExhale(resource, info))
        // exhale adaptively
        val condition = noPermission(predicate)
        emitConditional(condition, recursive, direct)
      case other =>
        emitExhale(other, info)
    }

  /**
   * Returns an expression representing the condition that there is no permission for the given location access.
   *
   * @param access The location access.
   * @return The expression.
   */
  private def noPermission(access: ast.ResourceAccess): ast.Exp = {
    val current = ast.CurrentPerm(access)()
    val write = ast.FullPerm()()
    ast.PermLtCmp(current, write)()
  }
}
