/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.solver

import fastparse.Parsed
import viper.silver.ast
import viper.silver.verifier.{ConstantEntry, ModelParser}

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.nio.file.Paths

/**
 * A solver.
 */
trait Solver {
  /**
   * Initializes the solver.
   */
  def initialize()

  /**
   * Solves the given constraints and returns a satisfying model.
   *
   * @param constraints The constraints to solve.
   * @return The model.
   */
  def solve(constraints: Seq[ast.Exp]): Map[String, Boolean]
}

/**
 * A Z3 solver.
 */
class Z3Solver(path: String) extends Solver {
  /**
   * The Z3 process.
   */
  private val process: Process = {
    // check whether z3 file exists and whether it is executable
    val file = Paths.get(path).toFile
    assert(file.isFile, s"$file is not a file.")
    assert(file.canExecute, s"$file is not executable.")

    // start process
    val builder = new ProcessBuilder(path, "-in")
    val process = builder.start()

    // add shutdown hook that destroys the process
    val hook = new Thread {
      override def run(): Unit =
        process.destroy()
    }
    Runtime.getRuntime.addShutdownHook(hook)

    process
  }

  /**
   * The reader used to read from Z3's output.
   */
  private val reader = {
    val inputStream = process.getInputStream
    val inputStreamReader = new InputStreamReader(inputStream)
    new BufferedReader(inputStreamReader)
  }

  /**
   * The writer used to write to Z3's input.
   */
  private val writer = {
    val outputStream = process.getOutputStream
    val outputStreamWriter = new OutputStreamWriter(outputStream)
    val bufferedWriter = new BufferedWriter(outputStreamWriter)
    new PrintWriter(bufferedWriter, true)
  }

  override def initialize(): Unit = {
    // set model format
    writeLine("(set-option :model.v2 true)")
  }

  override def solve(constraints: Seq[ast.Exp]): Map[String, Boolean] = {
    // enter new scope
    writeLine("(push)")

    // declare variables
    constraints
      .flatMap(_.collect { case ast.LocalVar(name, _) => name })
      .distinct
      .foreach { name => writeLine(s"(declare-const $name Bool)") }

    // emit constraints
    constraints.foreach { constraint =>
      val converted = convert(constraint)
      writeLine(s"(assert $converted)")
    }

    // solve constraints and process response
    writeLine("(check-sat)")
    val response = readResponse()
    val model = response match {
      case "sat" =>
        // get model
        writeLine("(get-model)")
        readModel()
      case _ => sys.error(s"Unexpected response: $response")
    }

    // leave scope
    writeLine("(pop)")
    // return model
    model
  }

  /**
   * Converts the given expression to an smt-lib string.
   *
   * @param expression The expression lib.
   * @return The converted expression.
   */
  private def convert(expression: ast.Exp): String =
    expression match {
      case ast.TrueLit() => "true"
      case ast.FalseLit() => "false"
      case ast.LocalVar(name, _) => name
      case ast.Not(argument) => s"(not ${convert(argument)})"
      case ast.And(left, right) => s"(and ${convert(left)} ${convert(right)})"
      case ast.Or(left, right) => s"(or ${convert(left)} ${convert(right)})"
      case ast.Implies(left, right) => s"(=> ${convert(left)} ${convert(right)})"
      case _ => sys.error(s"Unexpected expression: $expression")
    }

  /**
   * Writes the given line to Z3's input.
   *
   * @param line The input line.
   */
  private def writeLine(line: String): Unit =
    writer.println(line)

  /**
   * Reads a line from Z3's output.
   *
   * @return The output line.
   */
  private def readLine(): String =
    reader.readLine()

  /**
   * Reads a response from Z3's output.
   *
   * @return The response.
   */
  private def readResponse(): String = {
    var response = readLine()
    while (response.count(_ == '(') != response.count(_ == ')')) {
      response ++= "\n" ++ readLine()
    }
    response
  }

  /**
   * Reads a model from Z3's output.
   *
   * @return The model.
   */
  private def readModel(): Map[String, Boolean] = {
    // read response
    var response = readLine()
    while (!response.endsWith("\"")) {
      response ++= "\n" ++ readLine()
    }
    response = response.replace("\"", "")
    // parse response and create model
    fastparse.parse(response, ModelParser.model(_)) match {
      case Parsed.Success(model, _) =>
        model
          .entries
          .view
          .mapValues {
            case ConstantEntry("true") => true
            case ConstantEntry("false") => false
            case entry => sys.error(s"Unexpected model entry: $entry")
          }
          .toMap
      case _ =>
        sys.error("Unable to parse model.")
    }
  }
}
