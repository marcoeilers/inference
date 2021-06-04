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
import inference.builder.Builder
import inference.core.Placeholder
import inference.util.collections.Collections
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
    // return input
    val namespace = builder.namespace.copy()
    val placeholders = builder.placeholders.toSeq
    val methods = builder.methods.toMap
    new Input(configuration, namespace, processed, placeholders, methods)
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
 * @param namespace     The namespace.
 * @param program       The input program.
 * @param placeholders  The placeholders.
 * @param methods       A map from method names to its specification placeholders.
 */
class Input(val configuration: Configuration,
            val namespace: Namespace,
            val program: ast.Program,
            val placeholders: Seq[Placeholder],
            val methods: Map[String, (Placeholder, Placeholder)]) {
  /**
   * Lazily computed map from names to the corresponding placeholders.
   */
  private lazy val map =
    placeholders
      .map { placeholder => placeholder.name -> placeholder }
      .toMap

  /**
   * Returns the placeholder with the given name.
   *
   * @param name The name of the placeholder.
   * @return The placeholder.
   */
  def placeholder(name: String): Placeholder =
    map(name)
}

private class CheckBuilder extends Builder {
  /**
   * The buffer used to accumulate all specification placeholders.
   */
  val placeholders: mutable.Buffer[Placeholder] =
    ListBuffer.empty

  val methods: mutable.Buffer[(String, (Placeholder, Placeholder))] =
    ListBuffer.empty

  /**
   * The namespace used to generate unique identifiers.
   */
  val namespace: Namespace =
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
    // get unique name
    val unique = namespace.uniqueIdentifier(base)
    // create atomic predicates
    val atoms = {
      val references = parameters
        .filter(_.isSubtype(ast.Ref))
        .map(_.localVar)
      Collections
        .pairs(references)
        .map { case (a, b) => ast.NeCmp(a, b)() }
        .toSeq
    }
    // create placeholder
    val placeholder = Placeholder(unique, parameters, atoms, existing)
    placeholders.append(placeholder)
    placeholder
  }

  /**
   * Returns the given specification placeholder as a specification, i.e., an expression.
   *
   * @param placeholder The placeholder.
   * @return The specification.
   */
  private def makeSpecification(placeholder: Placeholder): ast.Exp = {
    val access = ast.PredicateAccess(placeholder.variables, placeholder.name)()
    ast.PredicateAccessPredicate(access, ast.FullPerm()())()
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
    method.body match {
      case Some(body) =>
        // create placeholder specifications
        val precondition = createPlaceholder(Names.precondition, method.formalArgs, method.pres)
        val postcondition = createPlaceholder(Names.postcondition, method.formalArgs ++ method.formalReturns, method.posts)
        methods.append(method.name -> (precondition, postcondition))
        // process body
        val processed = processSequence(body, method.formalArgs ++ method.formalReturns)
        // update method
        method.copy(
          pres = Seq(makeSpecification(precondition)),
          posts = Seq(makeSpecification(postcondition)),
          body = Some(processed)
        )(method.pos, method.info, method.errT)
      case _ =>
        sys.error("Methods without bodies are not supported yet.")
    }
  }

  /**
   * Processes the given sequence.
   *
   * @param sequence     The sequence to process.
   * @param declarations The declarations.
   * @return The processed sequence.
   */
  private def processSequence(sequence: ast.Seqn, declarations: Seq[ast.LocalVarDecl]): ast.Seqn = {
    // process statements
    val statements = scoped {
      val variables = sequence.scopedDecls.collect { case variable: ast.LocalVarDecl => variable }
      val updated = declarations ++ variables
      sequence.ss.foreach { statement => processStatement(statement, updated) }
    }
    // update sequence
    sequence.copy(ss = statements)(sequence.pos, sequence.info, sequence.errT)
  }

  /**
   * Processes the given statement.
   * TODO: Properly implement.
   *
   * @param statement    The statement to process.
   * @param declarations The declarations.
   */
  private def processStatement(statement: ast.Stmt, declarations: Seq[ast.LocalVarDecl]): Unit =
    statement match {
      case _ =>
        emit(statement)
    }
}
