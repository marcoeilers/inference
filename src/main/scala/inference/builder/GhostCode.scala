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

import scala.annotation.tailrec

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
   * @param expression  The expression to fold and exhale.
   * @param depth       The depth.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   * @param info        The info to attach to the fold or exhale statements.
   */
  protected def exhale(expression: ast.Exp, depth: Int)
                      (implicit hypothesis: Hypothesis, annotations: Seq[Annotation], info: ast.Info): Unit =
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
   * @param expression  The expression to fold.
   * @param depth       The depth.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   * @param info        The info to attach to the fold statements.
   */
  protected def fold(expression: ast.Exp, depth: Int)
                    (implicit hypothesis: Hypothesis, annotations: Seq[Annotation], info: ast.Info): Unit =
    if (depth > 0)
      process(expression) {
        case resource@ast.PredicateAccessPredicate(predicate, _) if configuration.useSegments && configuration.useAnnotations =>
          val strategy = getStrategy(predicate, annotations)
          foldWithStrategy(resource, depth, strategy)
        case other =>
          foldWithStrategy(other, depth, DefaultStrategy)
      }

  /**
   * Computes a fold strategy for the given predicate instance based on the given annotations.
   *
   * @param predicate   The predicate.
   * @param annotations The annotations.
   * @return The strategy.
   */
  private def getStrategy(predicate: ast.PredicateAccess, annotations: Seq[Annotation]): Strategy = {
    val strategies = annotations
      .map { annotation =>
        if (annotation.isAppend) {
          val condition = Expressions.makeEqual(predicate.args, annotation.arguments)
          val old = annotation.old(1)
          AppendStrategy(condition, old)
        } else {
          sys.error(s"Unsupported annotation: $annotation")
        }
      }
    // TODO: Handle more than one annotations?
    assert(strategies.length <= 1)
    strategies.headOption.getOrElse(DefaultStrategy)
  }

  /**
   * Recursively and adaptively folds the given expression up to the given depth using the given strategy.
   *
   * @param expression The expression to fold.
   * @param depth      The depth.
   * @param strategy   The strategy.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold statements.
   */
  private def foldWithStrategy(expression: ast.Exp, depth: Int, strategy: Strategy)
                              (implicit hypothesis: Hypothesis, info: ast.Info): Unit =
    if (depth > 0)
      process(expression) {
        case resource@ast.PredicateAccessPredicate(predicate, _) =>
          val condition = insufficient(resource)
          val body = {
            // default strategy
            val default = makeScope {
              // recursively fold nested predicate instances
              val instance = input.instance(predicate)
              val body = hypothesis.getBody(instance)
              foldWithStrategy(body, depth - 1, strategy)
              // fold predicate instance
              emitFold(resource, info)
            }
            // apply fold strategy
            strategy match {
              case DefaultStrategy =>
                default
              case AppendStrategy(_, old) =>
                // TODO: Use condition?
                // get predicate arguments
                val arguments = predicate.args
                val Seq(start, end) = arguments
                // condition under which to apply lemma
                val condition = {
                  val segment = Expressions.makeSegment(start, old)
                  sufficient(segment)
                }
                // lemma application
                val application = makeScope {
                  // get lemma instance
                  val instance = {
                    val name = Names.appendLemma
                    val arguments = Seq(start, old, end)
                    input.instance(name, arguments)
                  }
                  emitCall(instance)
                }
                Statements.makeConditional(condition, application, default)
              case other =>
                sys.error(s"Unexpected strategy: $other")
            }
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
   * Returns a condition capturing whether there are insufficient permissions for the given access.
   *
   * @param access     The access.
   * @param permission The permission amount.
   * @return The condition.
   */
  @tailrec
  private def insufficient(access: ast.Exp, permission: ast.Exp = ast.FullPerm()()): ast.Exp =
    access match {
      case ast.FieldAccessPredicate(field, permission) =>
        insufficient(field, permission)
      case ast.PredicateAccessPredicate(predicate, permission) =>
        insufficient(predicate, permission)
      case access: ast.ResourceAccess =>
        val current = ast.CurrentPerm(access)()
        ast.PermLtCmp(current, permission)()
    }

  /**
   * Returns a condition capturing whether there are sufficient permissions for the given access.
   *
   * @param access     The access.
   * @param permission The permission amount.
   * @return The condition.
   */
  @tailrec
  private def sufficient(access: ast.Exp, permission: ast.Exp = ast.FullPerm()()): ast.Exp =
    access match {
      case ast.FieldAccessPredicate(field, permission) =>
        sufficient(field, permission)
      case ast.FieldAccessPredicate(predicate, permission) =>
        sufficient(predicate, permission)
      case access: ast.ResourceAccess =>
        val current = ast.CurrentPerm(access)()
        ast.PermGeCmp(current, permission)()
    }

}

/**
 * A fold strategy.
 */
sealed trait Strategy

/**
 * The default fold strategy.
 */
case object DefaultStrategy extends Strategy

/**
 * The fold strategy allowing the use of the append lemma.
 *
 * @param condition The condition under which the lemma may be applied.
 * @param old       The old end parameter.
 */
case class AppendStrategy(condition: ast.Exp, old: ast.Exp) extends Strategy
