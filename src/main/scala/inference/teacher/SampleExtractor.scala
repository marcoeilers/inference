/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.{LowerBound, Record, Sample}
import inference.runner.Input
import inference.teacher.state.{Adaptor, ModelEvaluator, Snapshot, StateEvaluator}
import viper.silicon.interfaces.SiliconRawCounterexample
import viper.silver.ast
import viper.silver.verifier.VerificationError
import viper.silver.verifier.reasons.InsufficientPermission

/**
 * A sample extractor.
 */
trait SampleExtractor {
  /**
   * Type shorthand for counter examples.
   */
  private type Counter = SiliconRawCounterexample

  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Extracts a sample from the given query and verification error.
   *
   * @param query The query that caused the error.
   * @param error The verification error.
   */
  protected def extractSample(query: Query, error: VerificationError): Sample = {
    // extract counterexample and offending location
    val (counter, offending) = extractInformation(error)

    // get silicon state and model
    val siliconState = counter.state
    val model = ModelEvaluator(counter.model)

    // failing state
    val failState = StateEvaluator(None, siliconState, model)

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

    // create records
    val records = snapshots.map { snapshot =>
      val placeholder = snapshot.placeholder
      val adaptor = Adaptor(failState, snapshot)
      val resources = adaptor.adaptLocation(offending)
      Record(placeholder, resources)
    }

    // return lower bound sample
    assert(records.size == 1)
    LowerBound(records.head)
  }

  /**
   * Extracts information form the given verification error. The information consists of a counterexample and an
   * offending location.
   *
   * @param error The verification error.
   * @return The extracted information.
   */
  private def extractInformation(error: VerificationError): (Counter, ast.LocationAccess) = {
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
    // return information
    (counter, offending)
  }
}
