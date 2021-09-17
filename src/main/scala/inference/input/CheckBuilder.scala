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
import inference.core.{Kind, Placeholder}
import inference.util.Namespace
import inference.util.ast.SavedInfo
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A mixin providing check building capabilities.
 */
trait CheckBuilder extends Builder with Atoms {
  /**
   * The namespace used to generate unique identifiers.
   */
  private var namespace: Namespace = _

  /**
   * The list used to accumulate hints.
   */
  private var hints: mutable.Buffer[Hint] =
    ListBuffer.empty

  /**
   * The buffer used to accumulate the placeholders.
   */
  private val placeholders: mutable.Buffer[Placeholder] =
    ListBuffer.empty

  /**
   * The buffer used to accumulate the checks.
   */
  private val checks: mutable.Buffer[Check] =
    ListBuffer.empty

  /**
   * A map from method names to the corresponding pair of pre- and postconditions.
   */
  private var specifications: Map[String, (Placeholder, Placeholder)] = _

  /**
   * Resets the check builder.
   */
  private def reset(): Unit = {
    namespace = new Namespace()
    placeholders.clear()
    checks.clear()
    specifications = Map.empty
  }

  /**
   * Processes the given program.
   *
   * @param program The program to process.
   * @return The checks.
   */
  def buildChecks(configuration: Configuration, program: ast.Program): (Seq[Placeholder], Seq[Check]) = {
    // reset
    reset()
    // create placeholders for method specifications
    specifications = program
      .methods
      .map { method =>
        val name = method.name
        val arguments = method.formalArgs
        val declarations = arguments ++ method.formalReturns
        val precondition = createPlaceholder(name, Kind.Precondition, arguments, method.pres)
        val postcondition = createPlaceholder(name, Kind.Postcondition, declarations, method.posts)
        name -> (precondition, postcondition)
      }
      .toMap
    // process predicates
    program.predicates.foreach(processPredicate)
    // process methods
    program.methods.foreach(processMethod)
    // recursive predicate placeholder
    if (configuration.useRecursive) {
      val name = Names.recursive
      val names = if (configuration.useSegments) Seq("x", "y") else Seq("x")
      val parameters = names.map(ast.LocalVarDecl(_, ast.Ref)())
      createPlaceholder(name, Kind.Predicate, parameters, Seq.empty)
    }
    // add append lemma placeholder
    if (configuration.useSegments) {
      val name = Names.appendLemma
      val names = Seq("x", "y", "z")
      val parameters = names.map(ast.LocalVarDecl(_, ast.Ref)())
      val atoms = atomsFromParameters(parameters.slice(1, 2))
      val placeholder = Placeholder(name, Kind.Lemma, parameters, atoms, Seq.empty)
      placeholders.append(placeholder)
    }
    // return placeholders and checks
    (placeholders.toSeq, checks.toSeq)
  }

  /**
   * Creates a specification placeholder with the given name, kind, parameters, and existing specification. If necessary
   * the name is changed in order to ensure its uniqueness.
   *
   * @param name       The name (may change to ensure uniqueness).
   * @param kind       The kind of specification.
   * @param parameters The parameters.
   * @param existing   The existing specifications.
   * @return The placeholder.
   */
  private def createPlaceholder(name: String, kind: Kind.Value, parameters: Seq[ast.LocalVarDecl], existing: Seq[ast.Exp]): Placeholder = {
    // get unique name
    val unique = kind match {
      case Kind.Precondition => namespace.uniqueIdentifier(name = s"${Names.precondition}_$name", None)
      case Kind.Postcondition => namespace.uniqueIdentifier(name = s"${Names.postcondition}_$name", None)
      case Kind.Invariant => namespace.uniqueIdentifier(Names.invariant)
      case Kind.Predicate => name
    }
    // create atomic predicates
    val atoms = atomsFromParameters(parameters)
    // create placeholder
    val placeholder = Placeholder(unique, kind, parameters, atoms, existing)
    placeholders.append(placeholder)
    placeholder
  }

  /**
   * Processes the given predicate.
   *
   * @param predicate The predicate to process.
   */
  private def processPredicate(predicate: ast.Predicate): Unit = {
    val name = predicate.name
    val arguments = predicate.formalArgs
    val existing = predicate.body.toSeq
    createPlaceholder(name, Kind.Predicate, arguments, existing)
  }

  /**
   * Processes the given method.
   *
   * @param method The method to process.
   */
  private def processMethod(method: ast.Method): Unit =
    method.body match {
      case Some(body) =>
        // create placeholder specifications
        val name = method.name
        val (precondition, postcondition) = specifications(name)
        // process method body
        val (processed, _) = scopedHints {
          updateScope(body) {
            // inhale precondition
            instrumented(emitInhale(precondition.asResource))
            // process statements
            val declarations = method.formalArgs ++ method.formalReturns
            processStatements(body, declarations)
            // exhale postcondition
            instrumented(emitExhale(postcondition.asResource))
          }
        }
        // create check corresponding to method
        val check = MethodCheck(method, precondition, postcondition, processed)
        checks.append(check)
      case None =>
        sys.error("Methods without bodies are not supported.")
    }

  /**
   * Processes the given loop.
   *
   * @param loop         The loop to process.
   * @param declarations The declarations in scope.
   * @return The loop check.
   */
  private def processLoop(loop: ast.While, declarations: Seq[ast.LocalVarDecl]): LoopCheck = {
    // create placeholder specification
    val placeholder = createPlaceholder(Names.invariant, Kind.Invariant, declarations, loop.invs)
    // process loop body
    val body = loop.body
    val invariant = placeholder.asResource
    val (processed, _) = scopedHints {
      updateScope(body) {
        instrumented {
          emitInhale(invariant)
          emitInhale(loop.cond)
        }
        processStatements(body, declarations)
        instrumented(emitExhale(invariant))
      }
    }
    // create check corresponding to loop
    val name = namespace.uniqueIdentifier("loop")
    val check = LoopCheck(loop, name, placeholder, processed)
    checks.append(check)
    // return check
    check
  }

  /**
   * Processes the given sequence.
   *
   * @param sequence     The sequence to process.
   * @param declarations The declarations in scope.
   * @return The processed sequence.
   */
  private def processSequence(sequence: ast.Seqn, declarations: Seq[ast.LocalVarDecl]): ast.Seqn =
    updateScope(sequence)(processStatements(sequence, declarations))

  /**
   * Processes the statements of the given sequence while also taking into account the variables declared by the
   * sequence.
   *
   * @param sequence     The sequence.
   * @param declarations The declarations in scope.
   */
  def processStatements(sequence: ast.Seqn, declarations: Seq[ast.LocalVarDecl]): Unit = {
    // update declarations
    val updated = {
      val scoped = sequence
        .scopedDecls
        .collect { case variable: ast.LocalVarDecl => variable }
      declarations ++ scoped
    }
    // process statements
    sequence
      .ss
      .foreach { statement => processStatement(statement, updated) }
  }

  /**
   * Processes the given statement.
   *
   * @param statement    The statement to process.
   * @param declarations The declarations in scope.
   */
  def processStatement(statement: ast.Stmt, declarations: Seq[ast.LocalVarDecl]): Unit =
    statement match {
      case sequence: ast.Seqn =>
        // process sequence
        val processed = processSequence(sequence, declarations)
        emit(processed)
      case conditional@ast.If(condition, thenBranch, elseBranch) =>
        // process branches
        val (thenProcessed, thenHints) = scopedHints(processSequence(thenBranch, declarations))
        val (elseProcessed, elseHints) = scopedHints(processSequence(elseBranch, declarations))
        // update conditional
        val processed = conditional.copy(
          thn = thenProcessed,
          els = elseProcessed
        )(conditional.pos, conditional.info, conditional.errT)
        emit(processed)
        thenHints.foreach { hint => addHint(hint.withCondition(condition)) }
        elseHints.foreach { hint => addHint(hint.withCondition(ast.Not(condition)())) }
      case loop@ast.While(condition, _, _) =>
        // create check corresponding to loop
        val check = processLoop(loop, declarations)
        val invariant = check.invariant.asResource
        // instrument and cut loop
        instrumented(emitExhale(invariant))
        emitCut(check)
        instrumented {
          emitInhale(invariant)
          emitInhale(ast.Not(condition)())
        }
      case call@ast.MethodCall(name, arguments, targets) =>
        if (Names.isHint(name)) {
          // process hint
          val argument = arguments.head
          val old = save(argument)
          val hint = Hint(name, argument, old)
          addHint(hint)
        } else {
          // instrument method call
          instrumented {
            // make sure all arguments are variables and different from targets
            val variables = arguments.map {
              case variable: ast.LocalVar =>
                if (targets.contains(variable)) save(variable)
                else variable
              case access@ast.FieldAccess(receiver, _) =>
                // save value of field access and get receiver as a variable
                val variable = save(access)
                val old = asVariable(receiver)
                // add hint
                val hint = Hint(Names.downHint, variable, old)
                addHint(hint)
                // return variable
                variable
              case other =>
                sys.error(s"Unexpected argument: $other")
            }
            // exhale precondition and inhale postcondition
            val (precondition, postcondition) = specifications(name)
            emitExhale(precondition.asInstance(variables).asResource)
            emit(call)
            emitInhale(postcondition.asInstance(variables ++ targets).asResource)
          }
        }
      case ast.Inhale(resource: ast.PredicateAccessPredicate) =>
        // instrument inhale
        instrumented(emitInhale(resource))
      case ast.Exhale(resource: ast.PredicateAccessPredicate) =>
        // instrument exhale
        instrumented(emitExhale(resource))
      case _ =>
        emit(statement)
    }

  /**
   * Returns the result computed by the given function and also captures all hints produced during the computation.
   *
   * @param function The function computing the result.
   * @tparam R The type of the result.
   * @return The result and the collected hints.
   */
  private def scopedHints[R](function: => R): (R, Seq[Hint]) = {
    // save and reset hints
    val outer = hints
    val inner = ListBuffer.empty[Hint]
    hints = inner
    // compute result
    val result = function
    // restore hints and return result
    hints = outer
    (result, inner.toSeq)
  }

  /**
   * Instruments the statements emitted by the given expression.
   *
   * @param emitter The statement emitting expression.
   */
  private def instrumented(emitter: => Unit): Unit = {
    val body = makeScope(emitter)
    val statement = Instrumented(body, hints.toSeq)
    emit(statement)
  }

  /**
   * Returns the given expression as a variable.
   *
   * @param expression The expression.
   * @return The variable.
   */
  private def asVariable(expression: ast.Exp): ast.LocalVar =
    expression match {
      case variable: ast.LocalVar => variable
      case other => sys.error(s"Unexpected expression: $other")
    }

  /**
   * Saves the value of the given expression by assigning it to a local variable.
   *
   * @param expression The expression to save.
   * @return The local variable.
   */
  private def save(expression: ast.Exp): ast.LocalVar = {
    // create variable
    val name = namespace.uniqueIdentifier(Names.auxiliary)
    val info = SavedInfo
    val variable = ast.LocalVar(name, expression.typ)(info = info)
    // emit assignment and return variable
    emitAssignment(variable, expression)
    variable
  }

  /**
   * Adds the given hint.
   *
   * @param hint The hint to add.
   */
  private def addHint(hint: Hint): Unit =
    hints.append(hint)
}
