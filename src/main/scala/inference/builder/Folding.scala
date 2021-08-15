/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Hint, Input}
import inference.util.ast.{Expressions, Statements, ValueInfo}
import viper.silver.ast

/**
 * Mixin providing methods to fold and unfold specifications.
 */
trait Folding extends Builder {
  /**
   * Returns the input.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Unfolds the given expression up to the specified maximal depth.
   *
   * @param expression THe expression to unfold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The implicitly passed maximal depth.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param default    The implicitly passed default action applied to leaf expressions.
   */
  protected def unfold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                      (implicit maxDepth: Int, hypothesis: Hypothesis,
                       default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        unfold(left)
        unfold(right)
      case ast.Implies(guard, guarded) =>
        unfold(guarded, guards :+ guard)
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // unfold predicate if maximal depth is not reached yet
        val depth = Expressions.getDepth(predicate.args.head)
        if (depth < maxDepth) {
          // create unfolds
          val unfolds = makeScope {
            // unfold predicate
            emitUnfold(resource)
            // recursively unfold predicates appearing in body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            unfold(body)
          }
          // conditionally unfold
          emitConditional(guards, unfolds)
        } else {
          default(resource, guards)
        }
      case other =>
        default(other, guards)
    }

  /**
   * Folds the given expression under consideration of the given hints starting from the specified maximal depth.
   *
   * @param expression The expression to fold.
   * @param hints      The hints.
   * @param maxDepth   The implicitly passed maximal depth.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param default    The implicitly passed default action applied to leaf expressions.
   */
  protected def foldWithHints(expression: ast.Exp, hints: Seq[Hint])
                             (implicit maxDepth: Int, hypothesis: Hypothesis,
                              default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit = {
    /**
     * Helper method that handles the start argument of the predicate instances appearing in the given expression.
     *
     * @param expression The expression.
     * @param guards     The guards collected so far.
     */
    def handleStart(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty): Unit =
      expression match {
        case ast.And(left, right) =>
          handleStart(left, guards)
          handleStart(right, guards)
        case ast.Implies(guard, guarded) =>
          handleStart(guarded, guards :+ guard)
        case predicate: ast.PredicateAccessPredicate =>
          val start = predicate.loc.args.head
          val without: ast.Stmt = makeScope(fold(predicate))
          val body = hints.foldRight(without) {
            case (hint, result) =>
              // conditionally adapt fold depth
              val depth = if (hint.isDown) maxDepth - 1 else maxDepth + 1
              val adapted = makeScope(fold(predicate)(depth, hypothesis, default))
              // c under condition for hint relevance
              val condition = {
                val equality = ast.EqCmp(start, hint.argument)()
                Expressions.makeAnd(hint.conditions :+ equality)
              }
              Statements.makeConditional(condition, adapted, result)
          }
          emitConditional(guards, body)
        case other =>
          fold(other, guards)
      }

    // fold
    if (hints.isEmpty) fold(expression)
    else handleStart(expression)
  }

  /**
   * Folds the given expression starting from the specified maximal depth.
   *
   * TODO: Do we still need the default action?
   *
   * @param expression The expression to fold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The implicitly passed maximal depth.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param default    The implicitly passed default action applied to leaf expressions.
   */
  protected def fold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                    (implicit maxDepth: Int, hypothesis: Hypothesis,
                     default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        fold(left)
        fold(right)
      case ast.Implies(guard, guarded) =>
        fold(guarded, guards :+ guard)
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // fold predicate if maximal depth is not reached yet
        val depth = Expressions.getDepth(predicate.args.head)
        if (depth < maxDepth) {
          // create folds
          val folds = makeScope {
            // recursively fold predicates appearing in body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            fold(body)
            // fold predicate
            val info = ValueInfo(instance)
            emitFold(resource, info)
          }
          // conditionally fold
          emitConditional(guards, folds)
        } else {
          default(resource, guards)
        }
      case other =>
        default(other, guards)
    }
}
