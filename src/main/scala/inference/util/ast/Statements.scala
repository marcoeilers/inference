/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.ast

import viper.silver.ast

object Statements {
  /**
   * Returns a statement that havocs the given variables.
   *
   * @param variables The variables to havoc.
   * @return The statement.
   */
  def makeHavoc(variables: Seq[ast.LocalVar]): ast.Stmt = {
    val condition = ast.FalseLit()()
    val assignments = variables.map { variable => ast.LocalVarAssign(variable, variable)() }
    val body = ast.Seqn(assignments, Seq.empty)()
    ast.While(condition, Seq.empty, body)()
  }

  /**
   * Returns whether the given statement is guaranteed to be equivalent to a skip statement.
   *
   * @param statement The statement.
   * @return True if the statement is equivalent to a skip statement.
   */
  def isSkip(statement: ast.Stmt): Boolean =
    statement match {
      case ast.Seqn(statements, _) =>
        statements.forall(isSkip)
      case _ =>
        false
    }

  /**
   * Returns a skip statement.
   *
   * @return The skip statement.
   */
  @inline
  def makeSkip: ast.Seqn =
    makeSequence(Seq.empty)

  /**
   * Returns the given statement as a sequence.
   *
   * @param statement The statement.
   * @return The sequence.
   */
  def makeSequence(statement: ast.Stmt): ast.Seqn =
    statement match {
      case sequence: ast.Seqn => sequence
      case other => makeSequence(Seq(other))
    }

  /**
   * Returns a sequence with the given statements.
   *
   * @param statements The statements.
   * @param info       The info to attach to the sequence.
   * @return The sequence.
   */
  @inline
  def makeSequence(statements: Seq[ast.Stmt], info: ast.Info = ast.NoInfo): ast.Seqn =
    ast.Seqn(statements, Seq.empty)(info = info)

  /**
   * Returns the given sequence but with all the undeclared variables declared.
   *
   * @param sequence The sequence.
   * @param ignore   The declarations to ignore.
   * @return The sequence with no undeclared variables.
   */
  def makeDeclared(sequence: ast.Seqn, ignore: Seq[ast.Declaration] = Seq.empty): ast.Seqn = {
    // collect undeclared variables
    val undeclared = sequence
      .undeclLocalVars
      .map { variable => ast.LocalVarDecl(variable.name, variable.typ)() }
      .diff(ignore)
    // declare undeclared variables
    val declarations = sequence.scopedDecls ++ undeclared
    sequence.copy(scopedDecls = declarations)(sequence.pos, sequence.info, sequence.errT)
  }

  /**
   * Returns a conditional statement with the given condition, then branch, and else branch.
   *
   * @param condition  The condition.
   * @param thenBranch The then branch.
   * @param elseBranch The else branch.
   * @return The conditional statement.
   */
  def makeConditional(condition: ast.Exp, thenBranch: ast.Stmt, elseBranch: ast.Stmt): ast.If = {
    val thenBody = makeSequence(thenBranch)
    val elseBody = makeSequence(elseBranch)
    ast.If(condition, thenBody, elseBody)()
  }
}
