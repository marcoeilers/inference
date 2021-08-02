/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.Names
import inference.builder.Builder
import inference.core.Placeholder
import inference.util.Namespace
import inference.util.collections.Collections
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A mixin providing check building capabilities.
 */
trait CheckBuilder extends Builder {
  /**
   * The namespace used to generate unique identifiers.
   */
  var namespace: Namespace = _

  /**
   * Processes the given program.
   *
   * @param program The program to process.
   * @return The checks.
   */
  def buildChecks(configuration: Configuration, program: ast.Program): (Seq[Placeholder], Seq[Check]) = {
    // reset
    namespace = new Namespace()
    // initialize buffers
    implicit val placeholders: mutable.Buffer[Placeholder] = ListBuffer.empty
    implicit val checks: mutable.Buffer[Check] = ListBuffer.empty
    // process predicates
    program.predicates.foreach(processPredicate)
    // process methods
    program.methods.foreach(processMethod)
    // recursive predicate placeholder
    if (configuration.useRecursion()) {
      val name = Names.recursive
      val names = if (configuration.useSegments()) Seq("x", "y") else Seq("x")
      val parameters = names.map(ast.LocalVarDecl(_, ast.Ref)())
      createPlaceholder(name, parameters, Seq.empty)
    }
    // return placeholders and checks
    (placeholders.toSeq, checks.toSeq)
  }

  /**
   * Creates a specification placeholder with the given name, parameters, and existing specifications. In addition the
   * method may change the name in order to ensure its uniqueness.
   *
   * @param name         The name (may change to ensure uniqueness).
   * @param parameters   The parameters.
   * @param existing     The existing specifications.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   * @return The placeholder.
   */
  private def createUniquePlaceholder(name: String, parameters: Seq[ast.LocalVarDecl], existing: Seq[ast.Exp])(implicit placeholders: mutable.Buffer[Placeholder]): Placeholder = {
    // get unique name
    val unique = namespace.uniqueIdentifier(name)
    // create placeholder
    createPlaceholder(unique, parameters, existing)
  }

  /**
   * Creates a specification placeholder with the given name, parameters, and existing specifications.
   *
   * @param name         The name (may change to ensure uniqueness).
   * @param parameters   The parameters.
   * @param existing     The existing specifications.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   * @return The placeholder.
   */
  private def createPlaceholder(name: String, parameters: Seq[ast.LocalVarDecl], existing: Seq[ast.Exp])(implicit placeholders: mutable.Buffer[Placeholder]): Placeholder = {
    // create atomic predicates
    val atoms = {
      val references = parameters
        .filter(_.isSubtype(ast.Ref))
        .map(_.localVar)
      Collections
        .pairs(references)
        .map { case (first, second) => ast.NeCmp(first, second)() }
        .toSeq
    }
    // create placeholder
    val placeholder = Placeholder(name, parameters, atoms, existing)
    placeholders.append(placeholder)
    placeholder
  }

  /**
   * Processes the given predicate.
   *
   * @param predicate    The predicate to process.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   */
  private def processPredicate(predicate: ast.Predicate)(implicit placeholders: mutable.Buffer[Placeholder]): Unit = {
    val name = predicate.name
    val argument = predicate.formalArgs
    val existing = predicate.body.toSeq
    createPlaceholder(name, argument, existing)
  }

  /**
   * Processes the given method.
   *
   * @param method       The method to process.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   * @param checks       The implicitly passed buffer used to accumulate the checks.
   */
  private def processMethod(method: ast.Method)(implicit placeholders: mutable.Buffer[Placeholder], checks: mutable.Buffer[Check]): Unit =
    method.body match {
      case Some(body) =>
        // create placeholder specifications
        val arguments = method.formalArgs
        val declarations = arguments ++ method.formalReturns
        val precondition = createUniquePlaceholder(Names.precondition, arguments, method.pres)
        val postcondition = createUniquePlaceholder(Names.postcondition, declarations, method.posts)
        // process body
        val processed = processSequence(body, declarations)
        // create check corresponding to method
        val check = MethodCheck(method, precondition, postcondition, processed)
        checks.append(check)
      case None =>
        sys.error("Methods without bodies are not supported.")
    }

  /**
   * Processes the given sequence.
   *
   * @param sequence     The sequence to process.
   * @param declarations The declarations in scope.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   * @param checks       The implicitly passed buffer used to accumulate the checks.
   * @return The processed sequence.
   */
  private def processSequence(sequence: ast.Seqn, declarations: Seq[ast.LocalVarDecl])(implicit placeholders: mutable.Buffer[Placeholder], checks: mutable.Buffer[Check]): ast.Seqn = {
    // process statements
    val statements = scoped {
      val updated = declarations ++ sequence.scopedDecls.collect { case variable: ast.LocalVarDecl => variable }
      sequence.ss.foreach { statement => processStatement(statement, updated) }
    }
    // update sequence
    sequence.copy(ss = statements)(sequence.pos, sequence.info, sequence.errT)
  }

  /**
   * Processes the given statement.
   *
   * @param statement    The statement to process.
   * @param declarations The declarations in scope.
   * @param placeholders The implicitly passed buffer used to accumulate placeholders.
   * @param checks       The implicitly passed buffer used to accumulate the checks.
   */
  def processStatement(statement: ast.Stmt, declarations: Seq[ast.LocalVarDecl])(implicit placeholders: mutable.Buffer[Placeholder], checks: mutable.Buffer[Check]): Unit =
    statement match {
      case sequence: ast.Seqn =>
        // process sequence
        val processed = processSequence(sequence, declarations)
        emit(processed)
      case conditional@ast.If(_, thenBranch, elseBranch) =>
        // process branches
        val thenProcessed = processSequence(thenBranch, declarations)
        val elseProcessed = processSequence(elseBranch, declarations)
        // update conditional
        val processed = conditional.copy(
          thn = thenProcessed,
          els = elseProcessed
        )(conditional.pos, conditional.info, conditional.errT)
        emit(processed)
      case loop@ast.While(condition, existing, body) =>
        // create placeholder specification
        val invariant = createUniquePlaceholder(Names.invariant, declarations, existing)
        // process body
        val processed = processSequence(body, declarations)
        // create check corresponding to loop
        val name = namespace.uniqueIdentifier("loop")
        val check = LoopCheck(loop, name, invariant, processed)
        checks.append(check)
        // cut loop
        val cut = Cut(check)
        emit(cut)
      case _ =>
        emit(statement)
    }
}
