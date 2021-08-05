/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import com.typesafe.scalalogging.Logger
import inference.core._
import inference.input.{Configuration, Input}
import inference.teacher.state.{Adaptor, ModelEvaluator, Snapshot, StateEvaluator}
import inference.util.ast.{Expressions, Infos}
import viper.silicon.interfaces.SiliconRawCounterexample
import viper.silver.ast
import viper.silver.verifier.VerificationError
import viper.silver.verifier.reasons.InsufficientPermission

/**
 * A sample extractor mixin.
 */
trait SampleExtractor {
  /**
   * Type shorthand for counter examples.
   */
  private type Counter = SiliconRawCounterexample

  /**
   * Returns the logger.
   *
   * @return The logger.
   */
  protected def logger: Logger

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
   * Extracts a sample from the given query and verification error.
   *
   * @param query The query that caused the error.
   * @param error The verification error.
   */
  protected def extractSample(query: Query, error: VerificationError): Sample = {
    // extract counterexample and offending location
    val (counter, offending, info) = extractInformation(error)

    // get silicon state and model
    val siliconState = counter.state
    val model = ModelEvaluator(counter.model)

    // failing state
    val failState = StateEvaluator(None, siliconState, model)

    // get state snapshots
    val (failingSnapshot, otherSnapshots) = {
      // gather all encountered snapshots
      val snapshots = query
        .snapshots
        .flatMap {
          case (name, instance) if siliconState.oldHeaps.contains(name) =>
            val state = StateEvaluator(Some(name), siliconState, model)
            val snapshot = Snapshot(instance, state)
            Some(snapshot)
          case _ => None
        }
      // return current and other snapshots
      if (info.isDefined) {
        val current = snapshots.lastOption
        val others = snapshots.init
        (current, others)
      } else {
        val current = None
        (current, snapshots)
      }
    }

    /**
     * Helper method that computes a record corresponding to the given snapshot.
     *
     * @param snapshot The snapshot.
     * @return The record.
     */
    def recordify(snapshot: Snapshot): Record = {
      // adapt locations
      val locations = {
        val adaptor = Adaptor(failState, snapshot)
        adaptor.adaptLocation(offending)
      }
      // permission difference
      val amount = locations
        .headOption
        .map { formal =>
          val state = snapshot.state
          val actual = snapshot.instance.instantiate(formal)
          val instance = snapshot.instance
          val hypothesis = query.hypothesis
          evaluatePermission(actual, instance, state, hypothesis)
        }
        .getOrElse(0)
      // get placeholder and create abstraction
      val placeholder = snapshot.placeholder
      val abstraction = SnapshotAbstraction(snapshot)
      // create record
      if (query.isExhaled(snapshot.label)) {
        ExhaledRecord(placeholder, abstraction, locations, amount)
      } else {
        InhaledRecord(placeholder, abstraction, locations, amount)
      }
    }

    // create sample
    val sample = failingSnapshot match {
      // if there is a failing snapshot the error was caused by some specification
      case Some(snapshot) =>
        // compute record corresponding to failing specification
        val failing = recordify(snapshot)
        // if the failing specification exhales more than one permission we want to impose an upper bound, otherwise we
        // want to require the missing permission from an upstream specification
        if (failing.delta < -1) {
          // create upper bound sample
          UpperBound(failing)
        } else {
          // create implication sample
          val others = otherSnapshots.map(recordify)
          Implication(failing, LowerBound(others))
        }
      // if there is no failing snapshot the error was caused by some original program code
      case None =>
        val others = otherSnapshots.map(recordify)
        LowerBound(others)
    }

    // return sample
    logger.info(sample.toString)
    sample
  }

  /**
   * Extracts information form the given verification error. The information consists of a counterexample, an offending
   * location, and an optionally attached info value
   *
   * @param error The verification error.
   * @return The extracted information.
   */
  private def extractInformation(error: VerificationError): (Counter, ast.LocationAccess, Option[Instance]) = {
    // extract counterexample
    val counter = error.counterexample match {
      case Some(value: Counter) => value
      case Some(_) => sys.error("Unsupported counterexample.")
      case _ => sys.error("No counterexample.")
    }
    // extract offending location
    val offending = error.reason match {
      case InsufficientPermission(location) => location
      case reason => sys.error(s"Unexpected reason: $reason")
    }
    // extract attached info value
    val info = error.offendingNode match {
      case node: ast.Infoed => Infos.valueOption[Instance](node)
      case _ => None
    }
    // instantiate offending location
    val instantiated = info match {
      case Some(instance) =>
        if (configuration.noInlining()) instance.instantiate(offending)
        else offending
      case None =>
        offending
    }
    // return information
    (counter, instantiated, info)
  }

  /**
   * Evaluates the permission amount for the given offending location represented by the given specification placeholder
   * instance under consideration of the given state and hypothesis.
   *
   * @param offending  The offending location.
   * @param instance   The specification placeholder instance.
   * @param state      The state.
   * @param hypothesis The hypothesis.
   * @return The permission amount.
   */
  def evaluatePermission(offending: ast.LocationAccess, instance: Instance, state: StateEvaluator, hypothesis: Hypothesis): Int = {

    val maxDepth = Expressions.getDepth(offending)

    /**
     * Helper method that evaluates the permission amount for the offending location represented by the given
     * specification expression.
     *
     * @param expression The specification expression.
     * @return The permission amount.
     */
    def evaluate(expression: ast.Exp): Int =
      expression match {
        case ast.TrueLit() => 0
        case ast.And(left, right) =>
          val leftValue = evaluate(left)
          val rightValue = evaluate(right)
          leftValue + rightValue
        case ast.Implies(guard, guarded) =>
          val condition = state.evaluateBoolean(guard)
          if (condition) evaluate(guarded) else 0
        case ast.FieldAccessPredicate(access, _) =>
          offending match {
            case ast.FieldAccess(receiver, field) =>
              if (field == access.field) {
                val comparison = ast.EqCmp(receiver, access.rcv)()
                val condition = state.evaluateBoolean(comparison)
                if (condition) 1 else 0
              } else 0
            case _ =>
              ???
          }
        case ast.PredicateAccessPredicate(access, _) =>
          offending match {
            case offending: ast.FieldAccess =>
              val depth = Expressions.getDepth(access.args.head)
              if (depth < maxDepth) {
                val instance = input.instance(access)
                val body = hypothesis.getBody(instance)
                evaluate(body)
              } else 0
            case _ =>
              ???
          }
        case other =>
          sys.error(s"Unexpected expression: $other")
      }

    // call helper method
    val specification = hypothesis.getBody(instance)
    evaluate(specification)
  }
}
