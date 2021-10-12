/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Annotation, Check, Configuration, Input}
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
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

  /**
   * Returns the check currently being processed.
   *
   * @return The current check.
   */
  protected def check: Check

  /**
   * Returns the flag indicating whether specifications should be folded or exhaled.
   *
   * @return True if specifications should be exhaled instead of folded.
   */
  @deprecated
  protected def exhale: Boolean

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
      val depth = configuration.unfoldDepth
      recursiveUnfold(expression, depth)


    }

  /**
   * Folds or exhales the given expression.
   *
   * @param expression  The expression to fold or exhale.
   * @param simplify    The flag indicating whether the emitted code should be simplified.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   * @param info        The info to attach to fold or exhale statements.
   */
  protected def fold(expression: ast.Exp, simplify: Boolean = false)
                    (implicit hypothesis: Hypothesis, annotations: Seq[Annotation], info: ast.Info): Unit =
    if (simplify) simplified(fold(expression))
    else {
      // TODO: Use annotations
      val depth = configuration.foldDepth
      process(expression, reverse = true) {
        case resource: ast.PredicateAccessPredicate if depth > 0 =>
          recursiveFold(resource, depth)
          if (exhale) emitExhale(resource, info)
        case other =>
          if (exhale) emitExhale(other, info)
      }
    }

  /**
   * Recursively unfolds the given expression up to the given depth.
   * TODO: Use process method?
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
   * Recursively and adaptively folds the given expression up to the given depth.
   *
   * @param expression The expression to fold.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold or exhale statements.
   */
  private def recursiveFold(expression: ast.Exp, depth: Int)
                           (implicit hypothesis: Hypothesis, info: ast.Info): Unit = {
    if (depth > 0) process(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // conditionally fold predicate instance
        val condition = insufficient(resource)
        val body = makeScope {
          // recursively establish nested resources
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          recursiveFold(body, depth - 1)
          // fold predicate instance
          emitFold(resource, info)
        }
        emitConditional(condition, body)
      case resource: ast.FieldAccessPredicate =>
        // provoke a permission failure if there are insufficient permissions
        val condition = insufficient(resource)
        val body = makeScope(emitExhale(resource, info))
        emitConditional(condition, body)
    }
  }

  /**
   * Processes the resources appearing in the given expression by applying the given action to them.
   *
   * @param expression The expression to express.
   * @param reverse    The flag indicating whether the order of conjuncts should be reversed.
   * @param action     The action.
   */
  private def process(expression: ast.Exp, reverse: Boolean = false)(action: ast.Exp => Unit): Unit =
    expression match {
      case ast.And(left, right) =>
        if (reverse) {
          process(right, reverse)(action)
          process(left, reverse)(action)
        } else {
          process(left, reverse)(action)
          process(right, reverse)(action)
        }
      case ast.Implies(left, right) =>
        val processed = makeScope(process(right, reverse)(action))
        emitConditional(left, processed)
      case other =>
        action(other)
    }

  /**
   * Returns a condition capturing whether there are insufficient permissions for the given resource.
   *
   * @param resource The resource.
   * @return The condition.
   */
  private def insufficient(resource: ast.AccessPredicate): ast.Exp = {
    val current = ast.CurrentPerm(resource.loc)()
    ast.PermLtCmp(current, resource.perm)()
  }
}
