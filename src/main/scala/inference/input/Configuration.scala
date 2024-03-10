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
      arguments = arguments,
      inputOption = options.file.toOption,
      z3Exe = options.z3Exe(),
      useRecursive = isEqual(options.recursive, Set("default", "segments")),
      useSegments = isEqual(options.recursive, "segments"),
      infinite = options.infinite(),
      iterations = options.iterations(),
      escalation = options.escalation(),
      deescalation = options.deescalation(),
      maxLength = options.maxLength(),
      maxClauses = options.maxClauses(),
      unfoldDepth = options.unfoldDepth(),
      foldDelta = options.foldDelta(),
      useAnnotations = options.annotations(),
      useBatching = options.batching(),
      useNullityBranching = isEqual(options.branching, Set("nullity", "both")),
      useEqualityBranching = isEqual(options.branching, Set("equality", "both")),
      useUpperbounds = options.upperbounds(),
      useSyntacticBounds = isEqual(options.implicitBounds, "syntactic"),
      useSemanticBounds = isEqual(options.implicitBounds, "semantic"),
      querySimplification = options.querySimplification(),
      outputSimplification = options.outputSimplification(),
      choiceIntroduction = options.choiceIntroduction(),
      assumeConsolidation = isEqual(options.consolidation, "assume"),
      explicitConsolidation = isEqual(options.consolidation, "explicit"),
      nagini = options.nagini()
    )
  }

  /**
   * Helper method that checks whether the value of the given option is equal to one of the given values.
   *
   * @param option The option.
   * @param values The values.
   * @return True if the option's value is equal to one of the values.
   */
  @inline
  private def isEqual(option: ScallopOption[String], values: Set[String]) =
    satisfies(option, values.contains)

  /**
   * Helper method that checks whether the value of the given option is equal to the given value.
   *
   * @param option The option.
   * @param value  The value.
   * @return True if the option's value is equal to value.
   */
  @inline
  private def isEqual(option: ScallopOption[String], value: String) =
    satisfies(option, _ == value)

  /**
   * Helper method that checks whether the value of the given option satisfies the given predicate.
   *
   * @param option    The option.
   * @param predicate The predicate.
   * @return True if the value satisfies the predicate.
   */
  @inline
  private def satisfies(option: ScallopOption[String], predicate: String => Boolean): Boolean =
    option
      .filter(predicate)
      .isDefined

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

    val recursive: ScallopOption[String] =
      choice(
        name = "recursive",
        descr = "Enables the use of recursive predicates and predicate segments.",
        choices = Seq("none", "default", "segments"),
        default = Some("default")
      )

    val infinite: ScallopOption[Boolean] =
      toggle(
        name = "finite",
        descrYes = "Allows the generation of infinite predicates.",
        descrNo = "Disallows the generation of infinite predicates.",
        default = Some(false),
        hidden = true
      )

    val iterations: ScallopOption[Int] =
      opt[Int](
        name = "iterations",
        descr = "The number of iterations after which the learner gets exhausted and gives up.",
        default = Some(50)
      )

    val escalation: ScallopOption[Boolean] =
      toggle(
        name = "escalation",
        descrYes = "Enables template complexity escalation.",
        descrNo = "Disables template complexity escalation.",
        default = Some(true),
        hidden = true
      )

    val deescalation: ScallopOption[Boolean] =
      toggle(
        name = "deescalation",
        descrYes = "Enables template complexity deescalation before every iteration.",
        descrNo = "Disables template complexity deescalation.",
        default = Some(true),
        hidden = true
      )

    val maxLength: ScallopOption[Int] =
      opt[Int](
        name = "maxLength",
        descr = "The maximal length of access paths that may appear in specifications.",
        default = Some(3))

    val maxClauses: ScallopOption[Int] =
      opt[Int](
        name = "maxClauses",
        descr = "The maximal number of clauses that may be used per guard.",
        default = Some(1)
      )

    val unfoldDepth: ScallopOption[Int] =
      opt[Int](
        name = "unfoldDepth",
        descr = "The depth up to which predicates should be unfolded.",
        default = Some(1)
      )

    val foldDelta: ScallopOption[Int] =
      opt[Int](
        name = "foldDelta",
        descr = "The difference between the adaptive fold depth and the unfold depth.",
        default = Some(1)
      )

    val annotations: ScallopOption[Boolean] =
      toggle(
        name = "annotations",
        descrYes = "Enables the use of annotations.",
        descrNo = "Disables the use of annotations.",
        default = Some(true),
        hidden = true
      )

    val batching: ScallopOption[Boolean] =
      toggle(
        name = "batching",
        descrYes = "Enables batch processing of checks.",
        descrNo = "Disables batch processing of checks.",
        default = Some(true),
        hidden = true
      )

    val branching: ScallopOption[String] =
      choice(
        name = "branching",
        descr = "Enables nullity or equality branching.",
        choices = Seq("none", "nullity", "equality", "both"),
        default = Some("nullity"),
        hidden = true
      )

    val upperbounds: ScallopOption[Boolean] =
      toggle(
        name = "upperbounds",
        descrYes = "Enables upper bound samples.",
        descrNo = "Disables upper bound samples.",
        default = Some(false),
        hidden = true
      )

    val implicitBounds: ScallopOption[String] =
      choice(
        name = "implicit",
        descr = "Enables syntactic or semantic implicit upper bounds.",
        choices = Seq("none", "syntactic", "semantic"),
        default = Some("syntactic"),
        hidden = true
      )

    val querySimplification: ScallopOption[Boolean] =
      toggle(
        name = "querySimplification",
        descrYes = "Enables simplifications for queries.",
        descrNo = "Disables simplifications for queries.",
        default = Some(true),
        hidden = true
      )

    val outputSimplification: ScallopOption[Boolean] =
      toggle(
        name = "outputSimplification",
        descrYes = "Enables simplifications for extended program.",
        descrNo = "Disables simplifications for extended program.",
        default = Some(true),
        hidden = true)

    val choiceIntroduction: ScallopOption[Boolean] =
      toggle(
        name = "choiceIntroduction",
        descrYes = "Enables the introduction of choices for the second predicate argument.",
        descrNo = "Disables the introduction of choices for the second predicate argument.",
        default = Some(true),
        hidden = true
      )

    val consolidation: ScallopOption[String] =
      choice(
        name = "consolidation",
        descr = "Switches whether and how Silicon states are consolidated.",
        choices = Seq("none", "assume", "explicit"),
        default = Some("explicit"),
        hidden = true
      )

    val nagini: ScallopOption[Boolean] =
      toggle(
        name = "nagini",
        descrYes = "Enables Nagini mode.",
        descrNo = "Disables Nagini mode.",
        default = Some(false),
        hidden = true
      )

    val file: ScallopOption[String] =
      trailArg[String](
        name = "file",
        descr = "The path to the input file.",
        required = false,
      )

    verify()
  }
}

/**
 * A configuration object for the inference.
 *
 * @param arguments             The arguments corresponding to the configuration.
 * @param inputOption           The path to the input file.
 * @param z3Exe                 The path to the Z3 executable.
 * @param useRecursive          The flag indicating whether the use of recursive predicate is enabled.
 * @param useSegments           The flag indicating whether the us of predicate segments is enabled.
 * @param infinite              The flag indicating whether infinite recursive predicates are allowed.
 * @param iterations            The maximal number of iterations.
 * @param escalation            The flag indicating whether template complexity escalation is enabled.
 * @param deescalation          The flag indicating whether template complexity deescalation is enabled.
 * @param maxLength             The maximal length of access paths that may appear in specifications.
 * @param maxClauses            The maximal number of clauses that may be used per guard.
 * @param unfoldDepth           The depth up to which predicates should be unfolded.
 * @param foldDelta             The difference between the adaptive fold depth and the unfold depth.
 * @param useAnnotations        The flag indicating whether the use of annotations is enabled.
 * @param useBatching           The flag indicating whether batch processing of checks is enabled.
 * @param useNullityBranching   The flag indicating whether branching on nullity is enabled.
 * @param useEqualityBranching  The flag indicating whether branching on equality is enabled.
 * @param useUpperbounds        The flag indicating whether upper bound samples are enabled.
 * @param useSyntacticBounds    The flag indicating whether syntactic implicit upper bounds are enabled.
 * @param useSemanticBounds     The flag indicating whether semantic implicit upper bounds are enabled.
 * @param querySimplification   The flag indicating whether the simplification of queries is enabled.
 * @param outputSimplification  The flag indicating whether the simplification of output programs is enabled.
 * @param choiceIntroduction    The flag indicating whether the introduction of choices for the second predicate is enabled.
 * @param assumeConsolidation   The flag indicating whether state consolidation should be assumed.
 * @param explicitConsolidation The flag indicating whether state consolidation should be performed explicitly.
 * @param nagini                The flag indicating whether Nagini mode is enabled.
 */
case class Configuration(arguments: Seq[String],
                         inputOption: Option[String],
                         z3Exe: String,
                         useRecursive: Boolean,
                         useSegments: Boolean,
                         infinite: Boolean,
                         iterations: Int,
                         escalation: Boolean,
                         deescalation: Boolean,
                         maxLength: Int,
                         maxClauses: Int,
                         unfoldDepth: Int,
                         foldDelta: Int,
                         useAnnotations: Boolean,
                         useBatching: Boolean,
                         useNullityBranching: Boolean,
                         useEqualityBranching: Boolean,
                         useUpperbounds: Boolean,
                         useSyntacticBounds: Boolean,
                         useSemanticBounds: Boolean,
                         querySimplification: Boolean,
                         outputSimplification: Boolean,
                         choiceIntroduction: Boolean,
                         assumeConsolidation: Boolean,
                         explicitConsolidation: Boolean,
                         nagini: Boolean) {
  /**
   * Returns the path to the input file.
   *
   * @return The input file.
   */
  def input: String =
    inputOption match {
      case Some(file) => file
      case None => sys.error("No input file specified.")
    }

  /**
   * Returns a copy of this configuration with the input set to the given path.
   *
   * @param path The path to the input file.
   * @return The updated configuration.
   */
  def withInput(path: String): Configuration =
    copy(inputOption = Some(path))

  /**
   * Returns the depth up to which predicates should be adaptively folded.
   *
   * @return The fold depth.
   */
  def foldDepth: Int =
    unfoldDepth + foldDelta

  /**
   * Returns whether some kind of branching is enabled.
   *
   * @return True if some branching is enabled.
   */
  def useBranching: Boolean =
    useNullityBranching || useEqualityBranching

  /**
   * Returns whether the use of syntactic or semantic implicit upper bounds is enabled.
   *
   * @return True if implicit upper bounds are enabled.
   */
  def useImplicitBounds: Boolean =
    useSemanticBounds || useSyntacticBounds

  /**
   * Returns whether state consolidation is enabled.
   *
   * @return True if state consolidation is enabled.
   */
  def useConsolidation: Boolean =
    assumeConsolidation || explicitConsolidation
}
