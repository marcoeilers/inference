/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.PrintRunner

/**
 * The main object with the main method.
 */
object Main extends PrintRunner {
  /**
   *
   * The base options used if no arguments are specified.
   */
  val baseOptions: Seq[String] =
    Seq(
      "--z3Exe", "/usr/local/Viper/z3/bin/z3",
      "--simplifyQueries",
      "--simplifyExtended",
      "--verifyWithHints")

  /**
   * The options for an inference using heuristics.
   */
  val heuristicsOptions: Seq[String] =
    baseOptions ++ Seq("--useHeuristics")

  /**
   * The options for an inference using hints.
   */
  val hintsOptions: Seq[String] =
    baseOptions

  /**
   * The options for an inference using segments.
   */
  val segmentsOptions: Seq[String] =
    hintsOptions ++ Seq("--useSegments")

  /**
   * The input file used if no arguments are specified.
   */
  val file = "/Users/dohrau/Repositories/viper/inference/src/test/resources/example.vpr"

  /**
   * The main method, i.e., the entry point of the inference.
   *
   * @param arguments The arguments to the inference.
   */
  def main(arguments: Array[String]): Unit = {
    // inject default options if no arguments are specified
    val injected = if (arguments.nonEmpty) arguments.toSeq else segmentsOptions :+ file
    // run inference
    run(injected)
  }
}
