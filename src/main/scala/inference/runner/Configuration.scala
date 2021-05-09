/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.runner

import org.rogach.scallop.{ScallopConf, ScallopOption}

/**
 * A configuration obtained from parsing input arguments.
 *
 * @param arguments The arguments.
 */
class Configuration(arguments: Seq[String]) extends ScallopConf(arguments) {
  val z3Exe: ScallopOption[String] =
    opt[String](
      name = "z3Exe",
      descr = "The path to the z3 executable.")

  val file: ScallopOption[String] =
    trailArg[String](
      name = "file",
      descr = "The path to the input file.")

  verify()
}
