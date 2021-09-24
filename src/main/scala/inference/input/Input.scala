/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import fastparse.Parsed
import inference.Names
import inference.core.{Instance, Placeholder}
import viper.silver.ast
import viper.silver.parser._

import java.nio.file.{Files, Paths}
import scala.io.Source

/**
 * Input companion object.
 */
object Input extends CheckBuilder {
  /**
   * Constructs an input from the given configuration.
   *
   * @param configuration The configuration.
   * @return The input.
   */
  def fromConfiguration(configuration: Configuration): Input = {
    // parse input program
    val file = configuration.file
    val program = parse(file)
    // build checks
    val (placeholders, checks) = buildChecks(configuration, program)
    // return input
    new Input(program, configuration, placeholders, checks)
  }

  /**
   * Parses the given file.
   *
   * @param file The path to the file to parse.
   * @return The parsed program.
   */
  private def parse(file: String): ast.Program =
    parseOption(file) match {
      case Some(program) => program
      case None => sys.error(s"Unable to parse $file.")
    }

  /**
   * Optionally parses the given file.
   *
   * @param file The path to the file to parse.
   * @return The parsed program.
   */
  private def parseOption(file: String): Option[ast.Program] = {
    // read input
    val path = Paths.get(file)
    val stream = Files.newInputStream(path)
    val input = Source.fromInputStream(stream).mkString
    // parse program
    val result = FastParser.parse(input, path)
    val program = result match {
      case Parsed.Success(program: PProgram, _) if program.errors.isEmpty =>
        program.initProperties()
        Some(program)
      case _ =>
        None
    }
    // resolve and translate program
    program
      .flatMap { parsed => Resolver(beforeResolving(parsed)).run }
      .flatMap { resolved => Translator(resolved).translate }
      .map { translated => afterTranslating(translated) }
  }

  /**
   * Input transformation performed before the resolver is run.
   *
   * @param input The input.
   * @return The transformed input.
   */
  private def beforeResolving(input: PProgram): PProgram = {
    val methods = {
      // create dummy method for each hint
      val dummies = Names
        .annotations
        .map { hint =>
          val name = PIdnDef(hint)()
          val arguments = Seq(PFormalArgDecl(PIdnDef("x")(), TypeHelper.Ref)())
          PMethod(name, arguments, Seq.empty, Seq.empty, Seq.empty, None)()
        }
      input.methods ++ dummies
    }
    // update input program
    input.copy(methods = methods)(input.pos)
  }

  /**
   * Input transformation performed after the translator is run.
   *
   * @param input The input.
   * @return The transformed input.
   */
  private def afterTranslating(input: ast.Program): ast.Program = {
    // filter out dummy methods
    val methods = input
      .methods
      .filterNot { method => Names.isAnnotation(method.name) }
    // update input program
    input.copy(methods = methods)(input.pos, input.info, input.errT)
  }
}

/**
 * An input to the inference.
 *
 * @param program       The original input program.
 * @param configuration The configuration.
 * @param placeholders  The placeholders.
 * @param checks        The checks.
 */
class Input(val program: ast.Program,
            val configuration: Configuration,
            val placeholders: Seq[Placeholder],
            val checks: Seq[Check]) {

  /**
   * Map from names to the corresponding placeholder.
   */
  private val placeholderMap =
    placeholders
      .map { placeholder => placeholder.name -> placeholder }
      .toMap

  /**
   * Map from names to the corresponding check.
   */
  private val map =
    checks
      .map { check => check.name -> check }
      .toMap

  /**
   * Returns the checks as a sequence of batches that are meant to be checked as a group.
   *
   * @return The sequence of batches.
   */
  def batches: Seq[Seq[Check]] =
    if (configuration.useBatching) Seq(checks)
    else checks.map(Seq(_))

  /**
   * Returns the specification placeholder with the given name.
   *
   * @param name The name of the specification placeholder.
   * @return The specification placeholder.
   */
  def placeholder(name: String): Placeholder =
    placeholderMap(name)

  /**
   * Returns an instance of a specification placeholder with the given name and arguments.
   *
   * @param name      The name of the specification placeholder.
   * @param arguments The arguments.
   * @return The instance.
   */
  def instance(name: String, arguments: Seq[ast.Exp]): Instance =
    placeholder(name).asInstance(arguments)

  /**
   * Returns an instance of a specification placeholder corresponding to the given predicate.
   *
   * @param predicate The predicate.
   * @return The instance.
   */
  def instance(predicate: ast.PredicateAccess): Instance = {
    val name = predicate.predicateName
    val arguments = predicate.args
    instance(name, arguments)
  }

  /**
   * Returns the method check corresponding to the method with the given name.
   *
   * @param name The name of the method.
   * @return The method check.
   */
  def methodCheck(name: String): MethodCheck =
    map.get(name) match {
      case Some(check: MethodCheck) => check
      case _ => sys.error(s"No method check with name $name")
    }
}
