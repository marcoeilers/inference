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
   * Recursively unfolds the given expression up to the given depth.
   *
   * @param expression The expression to unfold.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   */
  protected def unfold(expression: ast.Exp, depth: Int)
                      (implicit hypothesis: Hypothesis): Unit =
    if (depth > 0) process(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // unfold predicate
        emitUnfold(resource)
        // recursively unfold nested predicate instances
        val instance = input.instance(predicate)
        val body = hypothesis.getBody(instance)
        unfold(body, depth - 1)
      case _ => // do nothing
    }

  /**
   * Recursively and adaptively folds the given expression up to the given depth and then exhales it.
   *
   * @param expression The expression to fold and exhale.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold or exhale statements.
   */
  protected def exhale(expression: ast.Exp, depth: Int)
                      (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    process(expression, reverse = true) {
      case resource: ast.PredicateAccessPredicate =>
        fold(resource, depth)
        emitExhale(resource, info)
      case other =>
        emitExhale(other, info)
    }

  /**
   * Recursively and adaptively folds the given expression up to the given depth.
   *
   * TODO: Use annotations!
   *
   * @param expression The expression to fold.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold statements.
   */
  protected def fold(expression: ast.Exp, depth: Int)
                    (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    if (depth > 0) process(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // conditionally fold predicate instance
        val condition = insufficient(resource)
        val body = makeScope {
          // recursively fold nested predicate instances
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          fold(body, depth - 1)
          // fold predicate instance
          emitFold(resource, info)
        }
        emitConditional(condition, body)
      case resource: ast.FieldAccessPredicate =>
        // TODO: Suppress in extended programs.
        // provoke a permission failure if there are insufficient permissions
        val condition = insufficient(resource)
        val body = makeScope(emitExhale(resource, info))
        emitConditional(condition, body)
      case _ => // do nothing
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
