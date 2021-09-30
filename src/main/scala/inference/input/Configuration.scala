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
 * Companion object used to create configuration objects.
 */
case object Configuration {
  /**
   * Returns a configuration object corresponding to the given input arguments.
   *
   * @param arguments The input arguments.
   * @return The configuration object.
   */
  def apply(arguments: Seq[String]): Configuration = {
    // parse input arguments
    val options = new OptionsParser(arguments)
    // create configuration object
    Configuration(
      file = options.file(),
      z3Exe = options.z3Exe(),
      useRecursive = options.recursive(),
      useSegments = options.segments(),
      iterations = options.iterations(),
      maxLength = options.maxLength(),
      maxClauses = options.maxClauses(),
      unfoldDepth = options.unfoldDepth(),
      foldDepth = options.foldDepth(),
      useBatching = options.batching(),
      useBranching = options.branching(),
      useUpperbounds = options.upperbounds(),
      useSyntacticBounds = options.syntacticBounds(),
      useSemanticBounds = options.semanticBounds(),
      simplifyQueries = options.simplifyQueries(),
      simplifyExtended = options.simplifyExtended(),
      stateConsolidation = options.stateConsolidation())
  }

  /**
   * A helper class used to parse input arguments.
   *
   * @param arguments The input arguments to parse.
   */
  private class OptionsParser(arguments: Seq[String]) extends ScallopConf(arguments) {
    val z3Exe: ScallopOption[String] =
      opt[String](
        name = "z3Exe",
        descr = "The path to the z3 executable.",
        default = Properties.envOrNone("Z3_EXE"),
        required = true
      )

    val recursive: ScallopOption[Boolean] =
      toggle(
        name = "recursive",
        descrYes = "Enables the use of recursive predicates.",
        descrNo = "Disables the use of recursive predicates.",
        default = Some(true)
      )

    val segments: ScallopOption[Boolean] =
      toggle(
        name = "segments",
        descrYes = "Enables the use of predicate segments.",
        descrNo = "Disables the use of predicate segments.",
        default = Some(false)
      )

    val iterations: ScallopOption[Int] =
      opt[Int](
        name = "iterations",
        descr = "The number of iterations after which the learner gets exhausted and gives up.",
        default = Some(20)
      )

    val maxLength: ScallopOption[Int] =
      opt[Int](
        name = "maxLength",
        descr = "The maximal length of access paths that may appear in specifications.",
        default = Some(2))

    val maxClauses: ScallopOption[Int] =
      opt[Int](
        name = "maxClauses",
        descr = "The maximal number of clauses that may appear in conditions.",
        default = Some(1)
      )

    val unfoldDepth: ScallopOption[Int] =
      opt[Int](
        name = "unfoldDepth",
        descr = "The depth up to which predicates should be unfolded.",
        default = Some(1)
      )

    val foldDepth: ScallopOption[Int] =
      opt[Int](
        name = "foldDepth",
        descr = "The depth up to which predicates should be folded.",
        default = Some(2)
      )

    val batching: ScallopOption[Boolean] =
      toggle(
        name = "batching",
        descrYes = "Enables batch processing of checks.",
        descrNo = "Disables batch processing of checks.",
        default = Some(true),
        hidden = true
      )

    val branching: ScallopOption[Boolean] =
      toggle(
        name = "branching",
        descrYes = "Enables branching on atomic predicates.",
        descrNo = "Disables branching on atomic predicates.",
        default = Some(true),
        hidden = true
      )

    val upperbounds: ScallopOption[Boolean] =
      toggle(
        name = "upperbounds",
        descrYes = "Enables upper bound samples.",
        descrNo = "Disables upper bound samples.",
        default = Some(true),
      )

    val syntacticBounds: ScallopOption[Boolean] =
      opt[Boolean](
        name = "syntacticBounds",
        descr = "Enables the use of syntactic implicit upper bounds.",
        hidden = true
      )

    val semanticBounds: ScallopOption[Boolean] =
      opt[Boolean](
        name = "semanticBounds",
        descr = "Enables the use of semantic implicit upper bounds.",
        hidden = true
      )

    val simplifyQueries: ScallopOption[Boolean] =
      toggle(
        name = "simplifyQueries",
        descrYes = "Enables simplifications for queries.",
        descrNo = "Disables simplifications for queries.",
        hidden = true
      )

    val simplifyExtended: ScallopOption[Boolean] =
      toggle(
        name = "simplifyExtended",
        descrYes = "Enables simplifications for extended program.",
        descrNo = "Disables simplifications for extended program.",
        hidden = true)

    val stateConsolidation: ScallopOption[Boolean] =
      toggle(
        name = "stateConsolidation",
        descrYes = "Enables Silicon's state consolidation.",
        descrNo = "Disables Silicon's state consolidation",
        default = Some(false),
        hidden = true
      )

    val file: ScallopOption[String] =
      trailArg[String](
        name = "file",
        descr = "The path to the input file."
      )

    validate(recursive, segments) { (recursive, segments) =>
      if (segments && !recursive) Left("Enabling predicate segments requires enabling recursive predicates.")
      else Right()
    }

    mutuallyExclusive(syntacticBounds, semanticBounds)

    verify()
  }
}

/**
 * A configuration object for the inference.
 *
 * @param file               The path to the input file.
 * @param z3Exe              The path to the Z3 executable.
 * @param useRecursive       The flag indicating whether the use of recursive predicate is enabled.
 * @param useSegments        The flag indicating whether the us of predicate segments is enabled.
 * @param iterations         The maximal number of iterations.
 * @param maxLength          The maximal length of access paths that may appear in specifications.
 * @param maxClauses         The maximal number of clauses that may appear in specifications.
 * @param unfoldDepth        The depth up to which predicates should be unfolded.
 * @param foldDepth          The depth up to which predicates should be folded.
 * @param useBatching        The flag indicating whether batch processing of checks is enabled.
 * @param useBranching       The flag indicating whether branching is enabled.
 * @param useUpperbounds     The flag indicating whether upper bound samples are enabled.
 * @param useSyntacticBounds The flag indicating whether syntactic implicit upper bounds are enabled.
 * @param useSemanticBounds  The flag indicating whether semantic implicit upper bounds are enabled.
 * @param simplifyQueries    The flag indicating whether the simplification of queries is enabled.
 * @param simplifyExtended   The flag indicating whether the simplification of extended programs is enabled.
 * @param stateConsolidation The flag indicating whether Silicon's state consolidation is enabled.
 */
case class Configuration(file: String,
                         z3Exe: String,
                         useRecursive: Boolean,
                         useSegments: Boolean,
                         iterations: Int,
                         maxLength: Int,
                         maxClauses: Int,
                         unfoldDepth: Int,
                         foldDepth: Int,
                         useBatching: Boolean,
                         useBranching: Boolean,
                         useUpperbounds: Boolean,
                         useSyntacticBounds: Boolean,
                         useSemanticBounds: Boolean,
                         simplifyQueries: Boolean,
                         simplifyExtended: Boolean,
                         stateConsolidation: Boolean) {
  /**
   * Returns whether the use of syntactic or semantic implicit upper bounds is enabled.
   *
   * @return True if implicit upper bounds are enabled.
   */
  def useImplicitBounds: Boolean =
    useSemanticBounds || useSyntacticBounds

  /**
   * TODO: Keep or remove.
   * Experimental flag that allows to introduce choices.
   *
   * @return True if additional choices may be introduced.
   */
  def introduceChoices: Boolean =
    false
}
