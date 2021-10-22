/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import scala.xml.Elem

/**
 * An object carrying statistical information.
 *
 * @param success       The flag indicating whether the inference was successful.
 * @param iterations    The number of iterations.
 * @param samples       The number of samples.
 * @param inputTime     The time it took to process the input.
 * @param startupTime   The time it took to start up the inference (mostly to create and start up verifier and solver).
 * @param inferenceTime The time recorded by the inference (this includes verifier and solver times).
 * @param verifierTimes The times recorded by the verifier.
 * @param solverTimes   The times recorded by the solver.
 */
case class Statistics(success: Boolean,
                      iterations: Int,
                      samples: Int,
                      inputTime: Long,
                      startupTime: Long,
                      inferenceTime: Long,
                      verifierTimes: Seq[Long],
                      solverTimes: Seq[Long]) {
  /**
   * Returns the total time.
   *
   * @return The total time.
   */
  def totalTime: Long =
    inputTime + startupTime + inferenceTime

  /**
   * Returns the total verifier time.
   *
   * @return The verifier time
   */
  def verifierTime: Long =
    verifierTimes.sum

  /**
   * Returns the total solver time.
   *
   * @return The solver time.
   */
  def solverTime: Long =
    solverTimes.sum

  /**
   * Returns the XML representation of the statistics.
   *
   * @return The XML element.
   */
  def toXml: Elem =
    <statistics>
      <success>{success}</success>
      <iterations>{iterations}</iterations>
      <samples>{samples}</samples>
      <times>
        <total>{formatTime(totalTime)}</total>
        <input>{formatTime(inputTime)}</input>
        <startup>{formatTime(startupTime)}</startup>
        <verifier>{formatTime(verifierTime)}</verifier>
        <solver>{formatTime(solverTime)}</solver>
      </times>
    </statistics>

  /**
   * Formats the given time.
   *
   * @param time The time to format.
   * @return The formatted time.
   */
  private def formatTime(time: Long): String = {
    val fractional = time / 1000.0
    String.format(f"$fractional%1.3f s")
  }
}
