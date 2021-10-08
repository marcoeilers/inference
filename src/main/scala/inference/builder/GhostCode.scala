/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.Names
import inference.core.Hypothesis
import inference.input.{Annotation, Check, Configuration, Input}
import inference.util.ast.{Expressions, Statements}
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
      val depth = configuration.foldDepth
      if (configuration.useSegments) {
        foldWithAnnotations(expression, annotations, depth)
      } else {
        foldWithoutAnnotations(expression, depth)
      }
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
   * Folds or exhales the given expression up to the given depth an potentially applies some lemmas depending on the
   * given annotations.
   *
   * @param expression  The expression to fold or exhale.
   * @param annotations The annotations.
   * @param depth       The depth.
   * @param hypothesis  The current hypothesis.
   * @param info        The info to attach to the fold or exhale statements.
   */
  private def foldWithAnnotations(expression: ast.Exp, annotations: Seq[Annotation], depth: Int)
                                 (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    process(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        val folds = {
          val without: ast.Stmt = makeScope(foldWithoutAnnotations(resource, depth))
          annotations.foldRight(without) {
            // handle append annotation
            case (annotation, result) if annotation.isAppend =>
              // get predicate arguments
              val arguments = predicate.args
              val Seq(start, end) = arguments
              // condition under which to apply the lemma
              val condition = {
                val inequality = ast.NeCmp(start, end)()
                val equalities = arguments
                  .zip(annotation.arguments)
                  .map { case (left, right) => ast.EqCmp(left, right)() }
                Expressions.makeAnd(annotation.conditions ++ Seq(inequality) ++ equalities)
              }
              // lemma application
              val application = makeScope {
                // get lemma instance
                val instance = {
                  val name = Names.appendLemma
                  val old = annotation.old(1)
                  val arguments = Seq(start, old, end)
                  input.instance(name, arguments)
                }
                // establish lemma precondition
                val precondition = hypothesis.getLemmaPrecondition(instance)
                foldWithoutAnnotations(precondition, depth)
                // apply lemma
                if (!exhale) {
                  emitCall(instance)
                }
              }
              // conditionally apply lemma
              Statements.makeConditional(condition, application, result)
            case (annotation, _) if annotation.isConcat =>
              // TODO: Implement me.
              ???
            case (other, _) =>
              sys.error(s"Unexpected annotation: $other")
          }
        }
        emit(folds)
      case other =>
        foldWithoutAnnotations(other, depth)
    }

  /**
   * Recursively folds or exhales the given expression up to the given depth.
   *
   * @param expression The expression to fold or exhale.
   * @param depth      The depth.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold or exhale statements.
   */
  private def foldWithoutAnnotations(expression: ast.Exp, depth: Int)
                                    (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    process(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) if depth > 0 =>
        val folds = {
          // action to perform if the predicate instance is present
          val thenBranch = makeScope {
            // recursively process instances appearing in the predicate body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            foldWithoutAnnotations(body, depth - 1)
            // fold predicate (unless we exhale everything)
            if (!exhale) {
              emitFold(resource, info)
            }
          }
          // action to perform if the predicate instance is not present
          val elseBranch = makeScope {
            if (exhale) {
              emitExhale(resource, info)
            }
          }
          // create conditional st
          val condition = noPermission(predicate)
          Statements.makeConditional(condition, thenBranch, elseBranch)
        }
        // only exhale predicate if it is framed. if it is not there will be a subsequent exhale of the missing
        // permission that we want to fail (since specifications are well-formed)
        if (exhale) {
          val framed = predicate
            .args
            .collect { case field: ast.FieldAccess => somePermission(field) }
          emitConditional(framed, folds)
        } else emit(folds)
      case other =>
        if (exhale) {
          emitExhale(other, info)
        }
    }

  /**
   * Processes the given expression by applying the given action to predicate instances and leaf expressions.
   *
   * @param expression The expression to process.
   * @param action     The action.
   */
  private def process(expression: ast.Exp)(action: ast.Exp => Unit): Unit =
    expression match {
      case ast.And(left, right) =>
        process(right)(action)
        process(left)(action)
      case ast.Implies(left, right) =>
        val processed = makeScope(process(right)(action))
        emitConditional(left, processed)
      case other =>
        action(other)
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
