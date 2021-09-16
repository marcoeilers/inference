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
      default = Some(20))

  val maxLength: ScallopOption[Int] =
    opt[Int](
      name = "maxLength",
      descr = "The maximal length of access paths that may appear in specifications.",
      default = Some(2))

  val maxClauses: ScallopOption[Int] =
    opt[Int](
      name = "maxClauses",
      descr = "The maximal number of clauses that may appear in conditions.",
      default = Some(1))

  val useHeuristics: ScallopOption[Boolean] =
    opt[Boolean](
      name = "useHeuristics",
      descr = "Explicitly forbids the us of hints.")

  @deprecated
  val useHints: ScallopOption[Boolean] =
    useHeuristics.map(!_)

  val simplifyQueries: ScallopOption[Boolean] =
    opt[Boolean](
      name = "simplifyQueries",
      descr = "Enables simplifications for queries.",
      hidden = true)

  val simplifyExtended: ScallopOption[Boolean] =
    opt[Boolean](
      name = "simplifyExtended",
      descr = "Enables simplifications for extended program.",
      hidden = true)

  val verifyWithHints: ScallopOption[Boolean] =
    opt[Boolean](
      name = "verifyWithHints",
      descr = "Enforces verification with hints.",
      hidden = true)

  val noRecursion: ScallopOption[Boolean] =
    opt[Boolean](
      name = "noPredicates",
      descr = "Disables the use of recursive predicates.")

  val useRecursion: ScallopOption[Boolean] =
    noRecursion.map(!_)

  val useSegments: ScallopOption[Boolean] =
    opt[Boolean](
      name = "useSegments",
      descr = "Enables the use of predicate segments.")

  val noBatching: ScallopOption[Boolean] =
    opt[Boolean](
      name = "noBatching",
      descr = "Disables batch verification of checks.",
      hidden = true)

  val noBranching: ScallopOption[Boolean] =
    opt[Boolean](
      name = "noBranching",
      descr = "Disables branching on accesses.",
      hidden = true)

  val useBranching: ScallopOption[Boolean] =
    noBranching.map(!_)

  val file: ScallopOption[String] =
    trailArg[String](
      name = "file",
      descr = "The path to the input file.")

  mutuallyExclusive(useSegments, noRecursion)

  verify()
}
