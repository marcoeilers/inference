/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.util.ast.{Expressions, Statements}
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A program builder.
 */
trait Builder {
  /**
   * The buffer used to accumulate the statements of the current scope.
   */
  private var scope: mutable.Buffer[ast.Stmt] = _

  /**
   * Returns all statements emitted by the given expression.
   *
   * @param emitter The statement emitting expression.
   * @return The emitted statements.
   */
  protected def scoped(emitter: => Unit): Seq[ast.Stmt] = {
    // save outer scope and create inner one
    val outer = scope
    val inner = ListBuffer.empty[ast.Stmt]
    scope = inner
    // emit statements
    emitter
    // restore outer scope and return statements
    scope = outer
    inner.toSeq
  }

  /**
   * Returns a sequence containing the statements emitted by the given expression. Moreover, this method makes sure that
   * all local variables are declared.
   *
   * @param emitter The statement emitting function.
   * @return The sequence.
   */
  protected def makeDeclaredScope(emitter: => Unit): ast.Seqn = {
    // create sequence
    val scope = makeScope(emitter)
    // collect undeclared variables
    val undeclared = scope
      .undeclLocalVars
      .map { variable => ast.LocalVarDecl(variable.name, variable.typ)() }
    // declare undeclared variables
    val declarations = scope.scopedDecls ++ undeclared
    scope.copy(scopedDecls = declarations)(scope.pos, scope.info, scope.errT)
  }

  /**
   * Returns a sequence containing the statements emitted by the given expression.
   *
   * @param emitter The statement emitting expression.
   * @return The sequence.
   */
  protected def makeScope(emitter: => Unit): ast.Seqn = {
    val statements = scoped(emitter)
    ast.Seqn(statements, Seq.empty)()
  }

  /**
   * Emits the given statement, i.e., adds the statement to the current scope.
   *
   * @param statement The statement to emit.
   */
  @inline
  protected def emit(statement: ast.Stmt): Unit =
    scope.append(statement)

  /**
   * Emits a statement that inhales the given expression.
   *
   * @param expression The inhaled expression.
   */
  protected def emitInhale(expression: ast.Exp): Unit = {
    val inhale = ast.Inhale(expression)()
    emit(inhale)
  }

  /**
   * Emits a statement that exhales the given expression.
   *
   * @param expression The exhaled expression.
   * @param info       The info to attach to the exhale statement.
   */
  protected def emitExhale(expression: ast.Exp, info: ast.Info = ast.NoInfo): Unit = {
    val exhale = ast.Exhale(expression)(info = info)
    emit(exhale)
  }

  /**
   * Emits a statement that unfolds the given resource.
   *
   * @param resource The resource to unfold.
   */
  protected def emitUnfold(resource: ast.PredicateAccessPredicate): Unit = {
    val unfold = ast.Unfold(resource)()
    emit(unfold)
  }

  /**
   * Emits a statement that conditionally executes the given body under the given conditions.
   *
   * @param conditions The conditions.
   * @param body       The body.
   */
  protected def emitConditional(conditions: Seq[ast.Exp], body: ast.Stmt): Unit = {
    val condition = Expressions.makeAnd(conditions)
    emitConditional(condition, body)
  }

  /**
   * Emits a statement that conditionally executes the given body under the given condition.
   *
   * @param condition The condition.
   * @param body      The body.
   */
  protected def emitConditional(condition: ast.Exp, body: ast.Stmt): Unit = {
    val sequence = Statements.makeSequence(body)
    val skip = Statements.makeSkip
    val conditional = ast.If(condition, sequence, skip)()
    emit(conditional)
  }

  /**
   * Emits a statement that folds the given resource.
   *
   * @param resource The resource to fold.
   * @param info     The info to attach to the fold statement.
   */
  protected def emitFold(resource: ast.PredicateAccessPredicate, info: ast.Info = ast.NoInfo): Unit = {
    val fold = ast.Fold(resource)(info = info)
    emit(fold)
  }

  /**
   * Emits an assignments that assigns the given value to a variable with the given name.
   *
   * @param name  The name of the variable.
   * @param value The value.
   */
  protected def emitAssignment(name: String, value: ast.Exp): Unit = {
    val target = ast.LocalVar(name, value.typ)()
    emitAssignment(target, value)
  }

  /**
   * Emits an assignment that assigns the given value to the given variable.
   *
   * @param target The variable.
   * @param value  The value.
   */
  protected def emitAssignment(target: ast.LocalVar, value: ast.Exp): Unit = {
    val assignment = ast.LocalVarAssign(target, value)()
    emit(assignment)
  }

  /**
   * Emits a label with the given name.
   *
   * @param name The name of the label.
   */
  protected def emitLabel(name: String): Unit = {
    val label = ast.Label(name, Seq.empty)()
    emit(label)
  }
}
