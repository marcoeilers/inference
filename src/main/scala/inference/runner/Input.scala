/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.runner

import fastparse.Parsed
import viper.silver.ast
import viper.silver.parser.{FastParser, PProgram, Resolver, Translator}

import java.nio.file.{Files, Paths}
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
    // return input
    Input(program)
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
 */
case class Input(program: ast.Program)
