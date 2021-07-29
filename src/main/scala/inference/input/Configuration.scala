/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.util.Properties

/**
 * A configuration obtained from parsing input arguments.
 *
 * @param arguments The arguments.
 */
class Configuration(arguments: Seq[String]) extends ScallopConf(arguments) {
  val z3Exe: ScallopOption[String] =
    opt[String](
      name = "z3Exe",
      descr = "The path to the z3 executable.",
      default = Properties.envOrNone("Z3_EXE"),
      required = true)

  val iterations: ScallopOption[Int] =
    opt[Int](
      name = "iterations",
      descr = "The number of iterations after which the learner gets exhausted and gives up.",
      default = Some(10))

  val noInlining: ScallopOption[Boolean] =
    opt[Boolean](
      name = "noInlining",
      descr = "Disables specification inlining",
      default = Some(true),
      hidden = true)

  val usePerm: ScallopOption[Boolean] =
    opt[Boolean](
      name = "usePerm",
      descr = "Use perm expressions to reflect on permission amounts.",
      default = Some(true),
      hidden = true)

  val file: ScallopOption[String] =
    trailArg[String](
      name = "file",
      descr = "The path to the input file.")

  verify()
}
