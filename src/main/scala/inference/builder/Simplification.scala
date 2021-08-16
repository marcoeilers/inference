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

/**
 * A mixin providing statement simplifications.
 */
trait Simplification extends Builder {
  /**
   * Simplifies all statements emitted by the given statement emitting expression and then re-emits them.
   *
   * @param emitter The statement emitting expression.
   */
  protected def simplified(emitter: => Unit): Unit = {
    val emitted = scoped(emitter)
    emitted.foreach { statement =>
      val simplified = simplifyStatement(statement)
      emit(simplified)
    }
  }

  /**
   * Simplifies the given statement.
   *
   * @param statement The statement to simplify.
   * @return The simplified statement.
   */
  private def simplifyStatement(statement: ast.Stmt): ast.Stmt =
    statement match {
      case sequence@ast.Seqn(statements, _) =>
        val simplified = statements.map(simplifyStatement)
        sequence.copy(ss = simplified)(sequence.pos, sequence.info, sequence.errT)
      case conditional@ast.If(condition, thenBranch, elseBranch) =>
        val simplified = simplifyCondition(Expressions.simplify(condition))
        simplified match {
          // drop else branch if condition simplifies to true
          case ast.TrueLit() =>
            simplifyStatement(thenBranch)
          // drop then branch if condition simplifies to false
          case ast.FalseLit() =>
            simplifyStatement(elseBranch)
          case _ =>
            val simplifiedThen = Statements.makeSequence(simplifyStatement(thenBranch))
            val simplifiedElse = Statements.makeSequence(simplifyStatement(elseBranch))
            ast.If(simplified, simplifiedThen, simplifiedElse)(conditional.pos, conditional.info, conditional.errT)
        }
      case _ =>
        statement
    }

  /**
   * Simplifies the given condition.
   *
   * @param condition The condition to simplify.
   * @return The simplified condition.
   */
  private def simplifyCondition(condition: ast.Exp): ast.Exp =
    condition
}
