/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.runner

import fastparse.Parsed
import inference.Names
import inference.core.Placeholder
import inference.util.Namespace
import viper.silver.ast
import viper.silver.parser.{FastParser, PProgram, Resolver, Translator}

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Input companion object.
 */
object Input {
  /**
   * Computes an input from the given configuration.
   *
   * @param configuration The configuration.
   * @return The input.
   */
  def apply(configuration: Configuration): Input = {
    // parse input program
    val file = configuration.file()
    val program = parse(file)
    // process program
    val builder = new CheckBuilder()
    val processed = builder.processProgram(program)
    val placeholders = builder.placeholders
    println(processed)
    // return input
    Input(configuration, processed, placeholders)
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
      .flatMap { parsed => Resolver(parsed).run }
      .flatMap { resolved => Translator(resolved).translate }
  }
}

/**
 * An input to the inference.
 *
 * @param configuration The configuration.
 * @param program       The input program.
 * @param placeholders  A map containing all placeholders.
 */
case class Input(configuration: Configuration, program: ast.Program, placeholders: Map[String, Placeholder])

private class CheckBuilder {
  /**
   * The buffer used to accumulate all specification placeholders.
   */
  private val buffer: mutable.Buffer[Placeholder] =
    ListBuffer.empty

  /**
   * The namespace used to generate unique identifiers.
   */
  private val namespace: Namespace =
    new Namespace()

  /**
   * Creates a specification placeholder with the given base name, parameters, and existing specifications.
   *
   * @param base       The base name.
   * @param parameters The parameters.
   * @param existing   The existing specification.
   * @return The placeholder.
   */
  private def createPlaceholder(base: String, parameters: Seq[ast.LocalVarDecl], existing: Seq[ast.Exp]): Placeholder = {
    val unique = namespace.uniqueIdentifier(base)
    val placeholder = Placeholder(unique, parameters, existing)
    buffer.append(placeholder)
    placeholder
  }

  /**
   * Returns the given specification placeholder as a specification, i.e., a sequence of expressions.
   *
   * @param placeholder The placeholder.
   * @return The specification.
   */
  private def makeSpecification(placeholder: Placeholder): Seq[ast.Exp] = {
    val access = ast.PredicateAccess(placeholder.variables, placeholder.name)()
    val resource = ast.PredicateAccessPredicate(access, ast.FullPerm()())()
    Seq(resource)
  }

  /**
   * Processes the given program.
   *
   * @param program The program to process.
   * @return The processed program.
   */
  def processProgram(program: ast.Program): ast.Program = {
    val methods = program.methods.map(processMethod)
    program.copy(methods = methods)(program.pos, program.info, program.errT)
  }

  /**
   * Processes the given method.
   *
   * @param method The method to process.
   * @return The processed method.
   */
  def processMethod(method: ast.Method): ast.Method = {
    // create placeholder specifications
    val precondition = createPlaceholder(Names.precondition, method.formalArgs, method.pres)
    val postcondition = createPlaceholder(Names.postcondition, method.formalArgs ++ method.formalReturns, method.posts)
    // update method
    method.copy(
      pres = makeSpecification(precondition),
      posts = makeSpecification(postcondition),
    )(method.pos, method.info, method.errT)
  }

  /**
   * Returns a map containing all specification placeholders.
   *
   * @return The map containing all placeholders.
   */
  def placeholders: Map[String, Placeholder] =
    buffer
      .map { placeholder => placeholder.name -> placeholder }
      .toMap
}
