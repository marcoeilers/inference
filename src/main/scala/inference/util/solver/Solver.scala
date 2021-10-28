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
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A solver.
 */
trait Solver {
  /**
   * Initializes the solver.
   */
  def initialize(): Unit

  /**
   * Adds the given constraint to the solver.
   *
   * @param constraint The constraint to add.
   */
  def addConstraint(constraint: ast.Exp): Unit

  /**
   * Adds the given comment.
   *
   * @param comment The comment to add.
   */
  def addComment(comment: String): Unit

  /**
   * Solves and clears the collected constraints.
   *
   * @return The model.
   */
  def solve(): Option[Map[String, Boolean]]
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

  /**
   * The buffer collecting the constraints and comments.
   */
  private val collected: mutable.Buffer[Either[ast.Exp, String]] =
    ListBuffer.empty

  override def initialize(): Unit = {
    // pick random seed to make things deterministic
    writeLine("(set-option :random-seed 0)")
    // set model format
    writeLine("(set-option :model.v2 true)")
  }

  override def addConstraint(constraint: ast.Exp): Unit =
    collected.append(Left(constraint))

  override def addComment(comment: String): Unit =
    collected.append(Right(comment))

  override def solve(): Option[Map[String, Boolean]] = {
    // enter new scope
    writeLine("(push)")

    // declare variables
    collected
      .flatMap {
        case Left(value) => value.collect { case variable: ast.LocalVar => variable }
        case Right(_) => Seq.empty
      }
      .distinct
      .foreach { case ast.LocalVar(name, typ) =>
        typ match {
          case ast.Bool => writeLine(s"(declare-const $name Bool)")
          case ast.Int => writeLine(s"(declare-const $name Int)")
        }
      }

    // emit constraints and comments
    collected.foreach {
      case Left(constraint) =>
        val converted = convert(constraint)
        writeLine(s"(assert $converted)")
      case Right(comment) =>
        writeLine(s"; $comment")
    }

    // clear collected constraints and comments
    collected.clear()

    // solve constraints and process response
    writeLine("(check-sat)")
    val response = readResponse()
    val model = response match {
      case "sat" =>
        // get model
        writeLine("(get-model)")
        val model = readModel()
        Some(model)
      case "unsat" =>
        None
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
      case ast.IntLit(value) => if (value < 0) s"(- ${-value})" else value.toString
      case ast.EqCmp(left, right) => s"(= ${convert(left)} ${convert(right)})"
      case ast.NeCmp(left, right) => s"(not (= ${convert(left)} ${convert(right)}))"
      case ast.GeCmp(left, right) => s"(>= ${convert(left)} ${convert(right)})"
      case ast.GtCmp(left, right) => s"(> ${convert(left)} ${convert(right)})"
      case ast.LeCmp(left, right) => s"(<= ${convert(left)} ${convert(right)})"
      case ast.LtCmp(left, right) => s"(< ${convert(left)} ${convert(right)})"
      case ast.CondExp(condition, left, right) => s"(ite ${convert(condition)} ${convert(left)} ${convert(right)})"
      case ast.Minus(argument) => s"(- ${convert(argument)})"
      case ast.Add(left, right) => s"(+ ${convert(left)} ${convert(right)})"
      case ast.Sub(left, right) => s"(- ${convert(left)} ${convert(right)})"
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
          .flatMap {
            case (name, ConstantEntry("true")) => Some(name -> true)
            case (name, ConstantEntry("false")) => Some(name -> false)
            case _ => None
          }
      case _ =>
        sys.error("Unable to parse model.")
    }
  }
}
