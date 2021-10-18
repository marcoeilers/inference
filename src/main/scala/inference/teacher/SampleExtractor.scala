/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import com.typesafe.scalalogging.Logger
import inference.Names
import inference.core.{Hypothesis, Instance}
import inference.core.sample._
import inference.input.{Configuration, Input}
import inference.teacher.state._
import inference.util.ast.InferenceInfo
import viper.silicon.interfaces.SiliconNativeCounterexample
import viper.silver.ast
import viper.silver.verifier.VerificationError
import viper.silver.verifier.reasons.InsufficientPermission

import scala.annotation.tailrec

/**
 * A sample extractor mixin.
 */
trait SampleExtractor {
  /**
   * Type shorthand for counter examples.
   */
  private type Counter = SiliconNativeCounterexample

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
   * Extracts a sample from the given framing query and verification error.
   *
   * @param query The framing query that caused the error.
   * @param error The verification error.
   * @return The extracted sample.
   */
  protected def extractFramingSample(query: Query, error: VerificationError): Sample = {
    // extract information from counter-example
    val counter = extractCounterExample(error)
    val offending = extractOffending(error)
    val reason = extractReason(error)

    // get label and instance
    val (label, instance) = {
      val heaps = counter.oldHeaps
      query
        .snapshots
        .filter { case (label, _) => heaps.contains(label) }
        .head
    }

    // compute state abstraction
    val state = {
      val state = StateEvaluator(label, counter)
      val snapshot = Snapshot(instance, state)
      StateAbstraction(snapshot)
    }
    val snapshot = Snapshot(instance, state.state)

    /**
     * Helper function that creates a lower bound for the given location.
     *
     * @param location The location.
     * @return The lower bound.
     */
    def lower(location: ast.LocationAccess): LowerBound = {
      val placeholder = instance.placeholder
      val adaptor = Adaptor(state.state, snapshot)
      val resource = abstractLocation(location, adaptor)
      val record = InhaledRecord(placeholder, state, resource, 0)
      LowerBound(record)
    }

    // create sample
    val sample = reason match {
      case Some(reason) =>
        // create implication sample
        val left = lower(reason)
        val right = lower(offending)
        Implication(left, right)
      case None =>
        // create lower bound sample
        lower(offending)
    }

    // return sample
    logger.info(sample.toString)
    sample
  }

  /**
   * Extracts a sample from the given basic query and verification error.
   *
   * @param query The basic query that caused the error.
   * @param error The verification error.
   * @return The extracted sample.
   */
  protected def extractBasicSample(query: Query, error: VerificationError): Sample = {
    println(error)
    // extract counter-example and offending location
    val counter = extractCounterExample(error)
    val offending = extractOffending(error)
    val info = extractInfo(error)

    // get model and store
    val model = ModelEvaluator(counter.model)
    val store = StateEvaluator.processStore(counter.store, model)

    // get state snapshots
    val (failingSnapshot, otherSnapshots) = {
      // gather all encountered snapshots
      val snapshots = query
        .snapshots
        .flatMap {
          case (label, instance) =>
            counter
              .oldHeaps
              .get(label)
              .map { native =>
                val heap = StateEvaluator.processHeap(native, model)
                val state = StateEvaluator(Some(label), store, heap, model)
                Snapshot(instance, state)
              }
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

    // failing state
    val failState = failingSnapshot match {
      case Some(snapshot) => snapshot.state
      case None =>
        val heap = StateEvaluator.processHeap(counter.heap, model)
        StateEvaluator(None, store, heap, model)
    }

    /**
     * Helper method that computes a record corresponding to the given snapshot.
     *
     * @param snapshot The snapshot.
     * @return The record.
     */
    def recordify(snapshot: Snapshot): Record = {
      // get placeholder and create abstractions
      val placeholder = snapshot.placeholder
      val state = StateAbstraction(snapshot)
      val resource = {
        val adaptor = Adaptor(failState, snapshot)
        abstractLocation(offending, adaptor)
      }
      // permission difference
      val amount = {
        val instance = snapshot.instance
        val actual = instance.instantiate(resource)
        val hypothesis = query.hypothesis
        val state = snapshot.state
        evaluatePermission(actual, instance, hypothesis, state)
      }
      // create record
      if (query.isExhaled(snapshot.label)) {
        ExhaledRecord(placeholder, state, resource, amount)
      } else {
        InhaledRecord(placeholder, state, resource, amount)
      }
    }

    // create sample
    val sample = failingSnapshot match {
      // if there is a failing snapshot the error was caused by some specification
      case Some(snapshot) =>
        // compute record corresponding to failing specification
        val failing = {
          recordify(snapshot) match {
            case ExhaledRecord(placeholder, state, resource, amount) =>
              val adapted = amount - 1
              assert(adapted <= 1)
              InhaledRecord(placeholder, state, resource, adapted)
            case _ =>
              sys.error(s"Inhaled specifications should not fail.")
          }
        }
        // if the failing specification exhales more than one permission (strict lower bound is at least 1) we might
        // want to impose an upper bound, otherwise we want to require the missing permission from an upstream
        // specification
        if (configuration.useUpperbounds && failing.delta >= 1) {
          // create upper bound sample
          UpperBound(failing)
        } else {
          // create implication sample
          val left = LowerBound(failing)
          val right = {
            val others = otherSnapshots.map(recordify)
            LowerBound(others)
          }
          Implication(left, right)
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
   * Extracts the counter-example from the given verification error.
   *
   * @param error The verification error.
   * @return The counter-example.
   */
  private def extractCounterExample(error: VerificationError): Counter = {
    // extract counter-examples
    val counters = error
      .failureContexts
      .flatMap(_.counterExample)
      .collect {
        case counter: Counter => counter
        case _ => sys.error(s"Unsupported counterexample")
      }
    // check that there is exactly one counter-example
    assert(counters.size == 1)
    counters.head
  }

  /**
   * Extracts the offending location from the given verification error.
   *
   * @param error The verification error.
   * @return The offending location.
   */
  private def extractOffending(error: VerificationError): ast.LocationAccess = {
    // get offending location
    val offending = error.reason match {
      case InsufficientPermission(offending) => offending
      case other => sys.error(s"Unexpected reason: $other")
    }
    // adapt offending location
    error.offendingNode match {
      case ast.Fold(ast.PredicateAccessPredicate(predicate, _)) =>
        val instance = input.instance(predicate)
        instance.instantiate(offending)
      case _ =>
        offending
    }
  }

  /**
   * Extracts the reason from the given verification error.
   *
   * @param error The verification error.
   * @return The reason.
   */
  private def extractReason(error: VerificationError): Option[ast.LocationAccess] = {
    /**
     * Helper method that strips conditions guarding the reason.
     *
     * @param expression The expression to strip.
     * @return The stripped reason.
     */
    @tailrec
    def stripGuards(expression: ast.Exp): Option[ast.LocationAccess] =
      expression match {
        case ast.Implies(_, right) =>
          stripGuards(right)
        case ast.PredicateAccessPredicate(predicate, _) =>
          assert(Names.isRecursive(predicate.predicateName))
          Some(predicate)
        case other =>
          None
      }

    // extract reason
    error.offendingNode match {
      case ast.Inhale(expression) => stripGuards(expression)
      case other => sys.error(s"Unexpected offending node: $other")
    }
  }

  /**
   * Extracts the info attached to the offending location from the given verification error.
   *
   * TODO: Is there a more elegant solution?
   *
   * @param error The verification error.
   * @return The info.
   */
  private def extractInfo(error: VerificationError): Option[ast.Info] =
    error.offendingNode match {
      case node: ast.Infoed =>
        node.info match {
          case info: InferenceInfo[Any] => Some(info)
          case ast.NoInfo => None
          case other => sys.error(s"Unexpected info: $other")
        }
      case _ => None
    }

  /**
   * Evaluates the permission amount for the given resource represented by the specification corresponding to the given
   * specification instance under consideration of the given hypothesis and state.
   *
   * @param resource   The resource.
   * @param instance   The specification instance.
   * @param hypothesis The hypothesis.
   * @param state      The state.
   * @return The permission amount.
   */
  def evaluatePermission(resource: ResourceAbstraction, instance: Instance, hypothesis: Hypothesis, state: StateEvaluator): Int = {
    val evaluator = new PermissionEvaluator(input, hypothesis, state)
    evaluator.evaluate(resource, instance, depth = 3)
  }

  /**
   * Returns an abstraction for the given resource.
   *
   * @param resource The resource to abstract.
   * @param adaptor  The adaptor.
   * @return The resource abstraction.
   */
  private def abstractLocation(resource: ast.LocationAccess, adaptor: Adaptor): ResourceAbstraction =
    resource match {
      case ast.FieldAccess(receiver, field) =>
        val abstraction = abstractAccess(receiver, adaptor, nullable = false)
        FieldAbstraction(abstraction, field)
      case ast.PredicateAccess(arguments, name) =>
        val abstractions = arguments.map { argument => abstractAccess(argument, adaptor, nullable = true) }
        PredicateAbstraction(name, abstractions)
    }

  /**
   * Returns an abstraction for the given access.
   *
   * @param access   The access to abstract.
   * @param adaptor  The adaptor.
   * @param nullable The flag indicating whether the abstraction may include the null reference.
   * @return The access abstraction.
   */
  private def abstractAccess(access: ast.Exp, adaptor: Adaptor, nullable: Boolean): AccessAbstraction = {
    val adapted = adaptor.adapt(access, nullable)
    adapted match {
      case Some(expressions) =>
        ExplicitSet(expressions)
      case None =>
        access match {
          case location: ast.FieldAccess => abstractLocation(location, adaptor)
          case other => sys.error(s"Unexpected access: $other")
        }
    }
  }
}
