/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

import inference.core.Hypothesis
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

/**
 * Mixin providing methods to fold and unfold specifications.
 */
trait Folding extends Builder {
  /**
   * Unfolds the given expression up to the specified maximal depth.
   *
   * @param expression THe expression to unfold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The maximal depth.
   * @param hypothesis The current hypothesis.
   * @param default    The default action applied to leaf expressions.
   */
  protected def unfold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                      (implicit maxDepth: Int, hypothesis: Hypothesis,
                       default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        unfold(left)
        unfold(right)
      case ast.Implies(guard, guarded) =>
        unfold(guarded, guards :+ guard)
      case ast.PredicateAccessPredicate(access, _) =>
        // TODO: Implement me.
        ???
      case other =>
        default(other, guards)
    }

  /**
   * Folds the given expression starting from the specified maximal depth.
   *
   * NOTE: The default action is used by the query builder to save permissions.
   *
   * @param expression The expression to fold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The maximal depth.
   * @param hypothesis The current hypothesis.
   * @param default    THe default action applied to leaf expressions.
   */
  protected def fold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                    (implicit maxDepth: Int, hypothesis: Hypothesis,
                     default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        fold(left)
        fold(right)
      case ast.Implies(guard, guarded) =>
        fold(guarded, guards :+ guard)
      case ast.PredicateAccessPredicate(access, _) =>
        // TODO: Implement me.
        ???
      case other =>
        default(other, guards)
    }
}
