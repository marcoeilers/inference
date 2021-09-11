/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.input.{Cut, LoopCheck}
import inference.util.ast.{Expressions, Statements}
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A program builder.
 */
trait Builder {
  /**
   * The magic field that enables the fold heuristics.
   */
  protected val magic: ast.Field =
    ast.Field("__CONFIG_HEURISTICS", ast.Bool)()

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
   * Returns the sequence obtained from the given sequence by replacing its statements with the statements emitted by
   * the given expression.
   *
   * @param original The original sequence.
   * @param emitter  THe statement emitting expression.
   * @return The sequence.
   */
  protected def updateScope(original: ast.Seqn)(emitter: => Unit): ast.Seqn = {
    val statements = scoped(emitter)
    original.copy(ss = statements)(original.pos, original.info, original.errT)
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
   * @param info       The info to attach to the inhale statement.
   */
  protected def emitInhale(expression: ast.Exp, info: ast.Info = ast.NoInfo): Unit = {
    val inhale = ast.Inhale(expression)(info = info)
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
   * Emits a conditional statement with the given condition and body.
   *
   * @param conditions The conditions.
   * @param body       The body.
   */
  protected def emitConditional(conditions: Seq[ast.Exp], body: ast.Stmt): Unit = {
    val condition = Expressions.makeAnd(conditions)
    emitConditional(condition, body)
  }

  /**
   * Emits a conditional statement with the given condition, then branch, and else branch.
   *
   * @param condition  The condition.
   * @param thenBranch The then branch
   * @param elseBranch The else branch.
   */
  protected def emitConditional(condition: ast.Exp, thenBranch: ast.Stmt, elseBranch: ast.Stmt = Statements.makeSkip): Unit = {
    val conditional = Statements.makeConditional(condition, thenBranch, elseBranch)
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

  /**
   * Emits a statement cutting out the loop corresponding to the given check.
   *
   * @param loop The loop check.
   */
  protected def emitCut(loop: LoopCheck): Unit = {
    val cut = Cut(loop)
    emit(cut)
  }
}
