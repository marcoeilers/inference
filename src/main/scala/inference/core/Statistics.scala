/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.input.Input

import scala.xml.Elem

/**
 * An object carrying statistical information.
 *
 * @param input         The input to the inference
 * @param hypothesis    The inferred hypothesis.
 * @param options       The options used by the inference.
 * @param success       The flag indicated whether the inference was successful.
 * @param iterations    The number of iterations.
 * @param samples       The number of samples.
 * @param totalTime     The total time.
 * @param teacherTime   The time spent by the teacher.
 * @param verifierTimes The times recorded by the verifier.
 * @param learnerTime   The time spent by the learner.
 * @param solverTimes   The times recorded by the solver.
 */
case class Statistics(input: Input,
                      hypothesis: Option[Hypothesis],
                      options: Seq[String],
                      success: Boolean,
                      iterations: Int,
                      samples: Int,
                      totalTime: Long,
                      teacherTime: Long,
                      verifierTimes: Seq[Long],
                      learnerTime: Long,
                      solverTimes: Seq[Long]) {
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
      <input>{input.path}</input>
      <options>{options.mkString(" ")}</options>
      <success>{success}</success>
      <iterations>{iterations}</iterations>
      <samples>{samples}</samples>
      <times>
        <total>{formatTime(totalTime)}</total>
        <teacher>{formatTime(teacherTime)}</teacher>
        <verifier>{formatTime(verifierTime)}</verifier>
        <learner>{formatTime(learnerTime)}</learner>
        <solver>{formatTime(learnerTime)}</solver>
      </times>
      <result>{hypothesis.toSeq.flatMap { hypothesis =>hypothesis.predicates.map { predicate => <p>{predicate.toString()}</p>}}}</result>
    </statistics>

  def formatBoolean(value: Boolean): String =
    if (value) "yes" else "no"

  /**
   * Formats the given time.
   *
   * @param time The time to format.
   * @return The formatted time.
   */
  def formatTime(time: Long): String = {
    val fractional = time / 1000.0
    String.format(f"$fractional%1.3f")
  }
}
