/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

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
   * Emits a label with the given name.
   *
   * @param name The name of the label.
   */
  protected def emitLabel(name: String): Unit = {
    val label = ast.Label(name, Seq.empty)()
    emit(label)
  }
}
