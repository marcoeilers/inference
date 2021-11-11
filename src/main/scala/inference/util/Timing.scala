/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A mixin providing timing functionality.
 */
trait Timing {
  /**
   * The buffer used to accumulate the recorded times.
   */
  private val recorded: mutable.Buffer[Long] =
    ListBuffer.empty

  /**
   * Returns the recorded times.
   *
   * @return The recorded times.
   */
  def times: Seq[Long] =
    recorded.toSeq

  /**
   * Returns the total time.
   *
   * @return The total time.
   */
  def totalTime: Long =
    times.sum

  /**
   * Computes the given value and records the elapsed time.
   *
   * @param value The value.
   * @tparam T The type of the value.
   * @return The value.
   */
  def recordTime[T](value: => T): T = {
    // compute result and record elapsed time
    val (result, elapsed) = time(value)
    recorded.append(elapsed)
    // return result
    result
  }

  /**
   * Computes the given value and returns it along the elapsed time.
   *
   * @param value The value.
   * @tparam T The type of the value.
   * @return The value and the elapsed time.
   */
  def time[T](value: => T): (T, Long) = {
    // compute result and measure elapsed time
    val start = System.currentTimeMillis
    val result = value
    val elapsed = System.currentTimeMillis - start
    // return result and elapsed time
    (result, elapsed)
  }
}
