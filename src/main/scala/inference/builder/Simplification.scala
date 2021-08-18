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
      val simplified = simplifyStatement(statement, Set.empty, Set.empty)
      emit(simplified)
    }
  }

  /**
   * Simplifies the given statement.
   *
   * @param statement The statement to simplify.
   * @param positive  The set of equalities known to be true.
   * @param negative  The set of equalities known to be false.
   * @return The simplified statement.
   */
  private def simplifyStatement(statement: ast.Stmt, positive: Set[ast.Exp], negative: Set[ast.Exp]): ast.Stmt =
    statement match {
      case sequence@ast.Seqn(statements, _) =>
        val simplified = statements.map { statement => simplifyStatement(statement, positive, negative) }
        sequence.copy(ss = simplified)(sequence.pos, sequence.info, sequence.errT)
      case conditional@ast.If(condition, thenBranch, elseBranch) =>
        // simplify condition and collect sets of positive and negative equalities
        val (simplified, collectedPositive, collectedNegative) = {
          val simpler = Expressions.simplify(condition)
          simplifyCondition(simpler, positive, negative)
        }
        // lazily compute simplified then branch
        lazy val simplifiedThen = {
          val updatedPositive = positive ++ collectedPositive
          val updatedNegative = negative ++ collectedNegative
          simplifyStatement(thenBranch, updatedPositive, updatedNegative)
        }
        // lazily compute simplified else branch
        lazy val simplifiedElse = {
          val updatedPositive = positive ++ collectedNegative
          val updatedNegative = negative ++ collectedPositive
          simplifyStatement(elseBranch, updatedPositive, updatedNegative)
        }
        // simplify conditional based on simplified condition
        simplified match {
          case ast.TrueLit() =>
            // drop else branch if condition simplifies to true
            simplifiedThen
          case ast.FalseLit() =>
            // drop then branch if condition simplifies to false
            simplifiedElse
          case _ =>
            // re-combine simplified branches
            val thenSequence = Statements.makeSequence(simplifiedThen)
            val elseSequence = Statements.makeSequence(simplifiedElse)
            if (Statements.isSkip(thenSequence) && Statements.isSkip(elseSequence)) Statements.makeSkip
            else ast.If(simplified, thenSequence, elseSequence)(conditional.pos, conditional.info, conditional.errT)
        }
      case _ =>
        statement
    }

  /**
   * Simplifies the given condition and simultaneously collects atoms known to be true or false.
   *
   * @param condition The condition to simplify.
   * @param positive  The set of equalities known to be true.
   * @param negative  The set of equalities known to be false.
   * @return A triple containing the simplified conditions, and two sets containing atoms known to be true and false,
   *         respectively.
   */
  private def simplifyCondition(condition: ast.Exp, positive: Set[ast.Exp], negative: Set[ast.Exp]): (ast.Exp, Set[ast.Exp], Set[ast.Exp]) =
    condition match {
      case atom@ast.EqCmp(left, right) =>
        val simplified = simplifyAtom(atom, positive, negative)
        val collected = equalities(left, right)
        (simplified, collected, Set.empty)
      case atom@ast.NeCmp(left, right) =>
        val simplified = simplifyAtom(atom, positive, negative)
        val collected = equalities(left, right)
        (simplified, Set.empty, collected)
      case ast.And(left, right) =>
        // simplify left and right conjunct
        val (simplifiedLeft, positiveLeft, negativeLeft) = simplifyCondition(left, positive, negative)
        val (simplifiedRight, positiveRight, negativeRight) = simplifyCondition(right, positive ++ positiveLeft, negative ++ negativeLeft)
        // return simplified conjunction and collected sets
        val simplified = ast.And(simplifiedLeft, simplifiedRight)()
        val collectedPositive = positiveLeft ++ positiveRight
        val collectedNegative = negativeLeft ++ negativeRight
        (simplified, collectedPositive, collectedNegative)
      case other =>
        (other, Set.empty, Set.empty)
    }

  /**
   * Simplifies the given atomic expression.
   *
   * @param atom     The atomic predicate to simplify.
   * @param positive The set of equalities known to be true.
   * @param negative The set of equalities known to be false.
   * @return The simplified atomic expression.
   */
  private def simplifyAtom(atom: ast.Exp, positive: Set[ast.Exp], negative: Set[ast.Exp]): ast.Exp =
    atom match {
      case equality: ast.EqCmp =>
        // simplify equality by checking whether it is contained in the positive or negative set
        if (positive.contains(equality)) ast.TrueLit()()
        else if (negative.contains(equality)) ast.FalseLit()()
        else equality
      case inequality@ast.NeCmp(left, right) =>
        // simplify inequality by negating it and flipping the role of positive and negative sets
        val negated = ast.EqCmp(left, right)()
        if (positive.contains(negated)) ast.FalseLit()()
        else if (negative.contains(negated)) ast.TrueLit()()
        else inequality
    }

  /**
   * Returns the set representing that the given expressions are equal.
   *
   * @param first  The first expression.
   * @param second The second expression.
   * @return The set of equalities.
   */
  private def equalities(first: ast.Exp, second: ast.Exp): Set[ast.Exp] = {
    val equality = ast.EqCmp(first, second)()
    val flipped = ast.EqCmp(second, first)()
    Set(equality, flipped)
  }
}
