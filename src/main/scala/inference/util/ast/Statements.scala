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
   * @return The sequence.
   */
  @inline
  def makeSequence(statements: Seq[ast.Stmt]): ast.Seqn =
    ast.Seqn(statements, Seq.empty)()
}
