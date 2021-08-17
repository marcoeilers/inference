/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Check, Configuration, Hint, Input}
import inference.util.ast.{Expressions, Statements, ValueInfo}
import viper.silver.ast

/**
 * Mixin providing methods to fold and unfold specifications.
 */
trait Folding extends Builder with Simplification {
  /**
   * Returns the input.
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
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

  /**
   * Returns whether hints should be used or not.
   *
   * @return True if hints should be used.
   */
  protected def useHints: Boolean

  /**
   * Unfolds the given expression.
   *
   * @param expression The expression to unfold.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param hints      The implicitly passed hints.
   */
  protected def unfold(expression: ast.Exp, simplify: Boolean = false)
                      (implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit =
    if (simplify) simplified(unfold(expression))
    else {
      implicit val maxDepth: Int =
        if (useHints) check.depth(hypothesis)
        else 0
      unfoldWithoutHints(expression)
    }

  /**
   * Folds the given expression.
   *
   * @param expression The expression to fold.
   * @param simplify   The flag indicating whether the emitted code should be simplified.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param hints      The implicitly passed hints.
   */
  protected def fold(expression: ast.Exp, simplify: Boolean = false)
                    (implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit =
    if (simplify) simplified(fold(expression))
    else if (useHints) {
      // fold with hints
      implicit val maxDepth: Int = check.depth(hypothesis)
      foldWithHints(expression, hints)
    } else {
      // fold without hints
      implicit val maxDepth: Int = configuration.heuristicsFoldDepth()
      foldWithoutHints(expression)
    }

  /**
   * Unfolds the given expression up to the specified maximal depth.
   *
   * @param expression THe expression to unfold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The implicitly passed maximal depth.
   * @param hypothesis The implicitly passed current hypothesis.
   * @param default    The implicitly passed default action applied to leaf expressions.
   */
  private def unfoldWithoutHints(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                                (implicit maxDepth: Int, hypothesis: Hypothesis,
                                 default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        unfoldWithoutHints(left)
        unfoldWithoutHints(right)
      case ast.Implies(guard, guarded) =>
        unfoldWithoutHints(guarded, guards :+ guard)
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
            unfoldWithoutHints(body)
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
  private def foldWithHints(expression: ast.Exp, hints: Seq[Hint])
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
          val without: ast.Stmt = makeScope(foldWithoutHints(predicate))
          val body = hints.foldRight(without) {
            case (hint, result) =>
              // conditionally adapt fold depth
              val depth = if (hint.isDown) maxDepth - 1 else maxDepth + 1
              val adapted = makeScope(foldWithoutHints(predicate)(depth, hypothesis, default))
              // c under condition for hint relevance
              val condition = {
                val equality = ast.EqCmp(start, hint.argument)()
                Expressions.makeAnd(hint.conditions :+ equality)
              }
              Statements.makeConditional(condition, adapted, result)
          }
          emitConditional(guards, body)
        case other =>
          foldWithoutHints(other, guards)
      }

    // fold
    if (hints.isEmpty) foldWithoutHints(expression)
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
  private def foldWithoutHints(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                              (implicit maxDepth: Int, hypothesis: Hypothesis,
                               default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        foldWithoutHints(left)
        foldWithoutHints(right)
      case ast.Implies(guard, guarded) =>
        foldWithoutHints(guarded, guards :+ guard)
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // fold predicate if maximal depth is not reached yet
        val depth = Expressions.getDepth(predicate.args.head)
        if (depth < maxDepth) {
          // create folds
          val folds = makeScope {
            // recursively fold predicates appearing in body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            foldWithoutHints(body)
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
