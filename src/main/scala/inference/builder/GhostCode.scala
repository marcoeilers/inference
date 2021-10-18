/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.Names
import inference.core.{Hypothesis, Instance}
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
    if (depth > 0)
      process(expression) {
        case resource@ast.PredicateAccessPredicate(predicate, _) =>
          // unfold predicate
          emitUnfold(resource)
          // recursively unfold nested predicate instances
          val instance = input.instance(predicate)
          val body = hypothesis.getBody(instance)
          unfold(body, depth - 1)
      }

  /**
   * Recursively and adaptively folds the given expression up to the given depth and then exhales it.
   *
   * @param expression  The expression to fold and exhale.
   * @param depth       The depth.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   * @param info        The info to attach to the fold or exhale statements.
   * @param exhaled     The flag indicating whether the expression is being exhaled.
   */
  protected def exhale(expression: ast.Exp, depth: Int)
                      (implicit hypothesis: Hypothesis,
                       annotations: Seq[Annotation],
                       info: ast.Info,
                       exhaled: Boolean = true): Unit =
    process(expression, reverse = true) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // fold predicate
        val strategy = getStrategy(predicate, annotations)
        implicit val exhaled: Boolean = true
        foldWithStrategy(resource, depth, strategy)
        // exhale predicate
        emitExhale(resource, info)
      case resource: ast.FieldAccessPredicate =>
        emitExhale(resource, info)
    }

  /**
   * Recursively and adaptively folds the given expression up to the given depth.
   *
   * @param expression  The expression to fold.
   * @param depth       The depth.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   * @param info        The info to attach to the fold statements.
   * @param exhaled     The flag indicating whether the expression is being exhaled.
   */
  protected def fold(expression: ast.Exp, depth: Int)
                    (implicit hypothesis: Hypothesis,
                     annotations: Seq[Annotation],
                     info: ast.Info,
                     exhaled: Boolean = false): Unit =
    processWithAdjustment(expression) {
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        val strategy = getStrategy(predicate, annotations)
        foldWithStrategy(resource, depth, strategy)
    }

  /**
   * Recursively and adaptively folds the given expression up to the given depth using the given strategy.
   *
   * @param expression The expression to fold.
   * @param depth      The depth.
   * @param strategy   The strategy.
   * @param hypothesis The current hypothesis.
   * @param info       The info to attach to the fold statements.
   * @param exhaled    The flag indicating whether the expression is being exhaled.
   */
  private def foldWithStrategy(expression: ast.Exp, depth: Int, strategy: Strategy)
                              (implicit hypothesis: Hypothesis, info: ast.Info, exhaled: Boolean): Unit =
    if (depth > 0)
      processWithAdjustment(expression) {
        case resource@ast.PredicateAccessPredicate(predicate, _) =>
          val foldCondition = Expressions.makeInsufficient(resource)
          val foldBody = {
            // default strategy
            val defaultStrategy = makeScope {
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
                defaultStrategy
              case strategy: LemmaStrategy =>
                // get predicate arguments
                val arguments = predicate.args
                val Seq(start, end) = arguments
                // get condition under which to apply lemma
                val condition = {
                  val triggers = strategy.triggers(start, end)
                  Expressions.makeAnd(triggers)
                }
                // get strategy using lemma
                val lemmaStrategy = makeScope {
                  val obligations = strategy.obligations(start, end)
                  // TODO: Obligations
                  // emit lemma application
                  val lemma = strategy.lemma(start, end)
                  emitCall(lemma)
                }
                // conditionally apply lemma
                Statements.makeConditional(condition, lemmaStrategy, defaultStrategy)
              case other =>
                sys.error(s"Unexpected strategy: $other")
            }
          }
          emitConditional(foldCondition, foldBody)
        case resource: ast.FieldAccessPredicate if exhaled =>
          // provoke a permission failure if there are insufficient permissions
          val condition = Expressions.makeInsufficient(resource)
          val body = makeScope(emitExhale(resource, info))
          emitConditional(condition, body)
      }

  /**
   * Processes the given expression by applying the given action to them. By default conjunctions are processed one
   * conjunct after another and implications are rewritten into conditionals.
   *
   * Moreover, if the expression is not being exhaled (indicated via implicitly passed flag), permissions for already
   * processed conjuncts are temporarily taken away in order for the permission introspection to work properly.
   *
   * @param expression The expression to process.
   * @param action     The action to apply.
   * @param exhaled    The flag indicating whether the expression is being exhaled.
   */
  def processWithAdjustment(expression: ast.Exp)(action: PartialFunction[ast.Exp, Unit])(implicit exhaled: Boolean): Unit =
    process(expression)(action.orElse {
      case ast.And(ast.And(first, second), right) if !exhaled =>
        // re-associate conjuncts
        val rewritten = ast.And(first, ast.And(second, right)())()
        processWithAdjustment(rewritten)(action)
      case ast.And(left, right) if !exhaled =>
        // process left conjunct
        processWithAdjustment(left)(action)
        // temporarily take away predicate instances appearing in the left conjunct
        val instances = collectInstances(left)
        instances.foreach(emitExhale(_))
        // process right conjunct
        processWithAdjustment(right)(action)
        // re-add predicate instances
        instances.foreach(emitInhale(_))
    })

  /**
   * Processes the given expression by applying the given action to them. By default conjunctions are processed one
   * conjunct after another and implications are rewritten into conditionals.
   *
   * @param expression The expression to process.
   * @param reverse    The flag indicating whether the order of conjuncts should be reversed.
   * @param action     The action to apply.
   */
  private def process(expression: ast.Exp, reverse: Boolean = false)(action: PartialFunction[ast.Exp, Unit]): Unit =
    action.applyOrElse[ast.Exp, Unit](expression, {
      case ast.And(left, right) =>
        // adjust order
        val (first, second) =
          if (reverse) (right, left)
          else (left, right)
        // process conjuncts
        process(first, reverse)(action)
        process(second, reverse)(action)
      case ast.Implies(left, right) =>
        val processed = makeScope(process(right, reverse)(action))
        emitConditional(left, processed)
      case _ =>
    })

  /**
   * Computes a fold strategy for the given predicate instance based on the given annotations.
   *
   * @param predicate   The predicate.
   * @param annotations The annotations.
   * @return The strategy.
   */
  private def getStrategy(predicate: ast.PredicateAccess, annotations: Seq[Annotation]): Strategy =
    if (configuration.useSegments && configuration.useAnnotations) {
      val strategies = annotations
        .map { annotation =>
          if (annotation.isAppend) {
            val equality = {
              val segmentArguments = annotation.arguments
              Expressions.makeEqual(predicate.args, segmentArguments)
            }
            val condition = ast.And(annotation.flag, equality)()
            val middle = annotation.saved(1)
            AppendStrategy(condition, middle)
          } else if (annotation.isConcat) {
            val equality = {
              val arguments = annotation.arguments
              val first = arguments.head
              val third = arguments(2)
              val segmentArguments = Seq(first, third)
              Expressions.makeEqual(predicate.args, segmentArguments)
            }
            val condition = ast.And(annotation.flag, equality)()
            val middle = annotation.saved(1)
            ConcatStrategy(condition, middle)
          } else {
            sys.error(s"Unsupported annotation: $annotation")
          }
        }
      // TODO: Handle more than one annotation?
      assert(strategies.length <= 1)
      strategies.headOption.getOrElse(DefaultStrategy)
    } else {
      DefaultStrategy
    }

  /**
   * Collects all predicate instances appearing in the given expression.
   *
   * @param expression The expression.
   * @return The predicate instances.
   */
  private def collectInstances(expression: ast.Exp): Seq[ast.Exp] =
    expression match {
      case ast.And(left, right) =>
        val leftInstances = collectInstances(left)
        val rightInstances = collectInstances(right)
        leftInstances ++ rightInstances
      case ast.Implies(left, right) =>
        val inner = collectInstances(right)
        inner.map { instance => ast.Implies(left, instance)() }
      case resource: ast.PredicateAccessPredicate =>
        Seq(resource)
      case _ =>
        Seq.empty
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
   * A strategy that allows the use of a lemma.
   */
  trait LemmaStrategy extends Strategy {
    /**
     * The condition that triggers the use of the lemma for the segment with the given start and end parameters.
     *
     * @param start      The start parameter.
     * @param end        The end parameter.
     * @param hypothesis The current hypothesis.
     * @return The trigger conditions.
     */
    def triggers(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp]

    /**
     * The obligations for the use of the lemma for the segment with the given start and end parameters. Obligations are
     * essentially the difference between the triggers and the lemma preconditions.
     *
     * @param start      The start parameter.
     * @param end        The end parameter.
     * @param hypothesis The current hypothesis.
     * @return The obligations.
     */
    def obligations(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp]

    /**
     * The lemma instance that may be used for the segment with the given start and end parameters.
     *
     * @param start The start parameter.
     * @param end   The end parameter.
     * @return The lemma instance.
     */
    def lemma(start: ast.Exp, end: ast.Exp): Instance
  }

  /**
   * A strategy that allows the use of the append lemma.
   *
   * @param condition The condition under which the lemma may be applied.
   * @param middle    The middle parameter of the lemma.
   */
  case class AppendStrategy(condition: ast.Exp, middle: ast.Exp) extends LemmaStrategy {
    override def triggers(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp] = {
      // require sufficient permission for the segment from the start to the middle
      val sufficient = {
        val segment = Expressions.makeSegment(start, middle)
        Expressions.makeSufficient(segment)
      }
      // get link conditions
      val links = {
        val arguments = Seq(middle, end)
        val instance = input.instance(Names.recursive, arguments)
        hypothesis
          .getLinks(instance)
          .map { link => ast.EqCmp(link, end)() }
      }
      // combine all conditions
      Seq(condition, sufficient) ++ links
    }

    override def obligations(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp] =
      Seq.empty

    override def lemma(start: ast.Exp, end: ast.Exp): Instance = {
      val arguments = Seq(start, middle, end)
      input.instance(Names.appendLemma, arguments)
    }
  }

  /**
   * A strategy that allows the use of the concat lemma.
   *
   * @param condition The condition under which the lemma may be applied.
   * @param middle    The middle parameter of the lemma.
   */
  case class ConcatStrategy(condition: ast.Exp, middle: ast.Exp) extends LemmaStrategy {
    override def triggers(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp] = {
      val segment = Expressions.makeSegment(start, middle)
      val sufficient = Expressions.makeSufficient(segment)
      Seq(condition, sufficient)
    }

    override def obligations(start: ast.Exp, end: ast.Exp)(implicit hypothesis: Hypothesis): Seq[ast.Exp] = {
      val segment = Expressions.makeSegment(middle, end)
      val resource = Expressions.makeResource(segment)
      Seq(resource)
    }

    override def lemma(start: ast.Exp, end: ast.Exp): Instance = {
      val arguments = Seq(start, middle, end)
      input.instance(Names.concatLemma, arguments)
    }
  }
}
