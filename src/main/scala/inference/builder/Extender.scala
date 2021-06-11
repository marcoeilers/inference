/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.Input
import viper.silver.ast

/**
 * An extender.
 */
trait Extender extends Builder {
  /**
   * Extends the program given by the input with specifications corresponding to the given hypothesis.
   *
   * @param input      The input to the inference.
   * @param hypothesis The inferred hypothesis.
   * @return The extended program.
   */
  def extend(implicit input: Input, hypothesis: Hypothesis): ast.Program = {
    // get program
    val program = input.program
    // extend methods
    val methods = program
      .methods
      .map(extendMethod)
    // update program
    program.copy(
      methods = methods
    )(program.pos, program.info, program.errT)
  }

  /**
   * Extends the given method.
   *
   * @param method     The method to extend.
   * @param hypothesis The implicitly passed hypothesis.
   * @return The extended method.
   */
  private def extendMethod(method: ast.Method)(implicit input: Input, hypothesis: Hypothesis): ast.Method = {
    // get method specification
    val name = method.name
    val precondition = hypothesis.getBody(input.precondition(name).asInstance)
    val postcondition = hypothesis.getBody(input.postcondition(name).asInstance)
    // extend method body
    // TODO: Extend check instead of loop body.
    val body = method.body.map(extendSequence)
    // update method
    method.copy(
      pres = Seq(precondition),
      posts = Seq(postcondition),
      body = body
    )(method.pos, method.info, method.errT)
  }

  /**
   * Extends the givens sequence.
   *
   * @param sequence   The sequence to extend.
   * @param hypothesis The implicitly passed hypothesis.
   * @return The extended sequence.
   */
  private def extendSequence(sequence: ast.Seqn)(implicit hypothesis: Hypothesis): ast.Seqn = {
    val statements = scoped(sequence.ss.foreach(extendStatement))
    sequence.copy(ss = statements)(sequence.pos, sequence.info, sequence.errT)
  }

  /**
   * Extends the given statement.
   *
   * @param statement  The statement to extend.
   * @param hypothesis The implicitly passed hypothesis.
   */
  private def extendStatement(statement: ast.Stmt)(implicit hypothesis: Hypothesis): Unit =
    statement match {
      case sequence: ast.Seqn =>
        val extended = extendSequence(sequence)
        emit(extended)
      case conditional: ast.If =>
        // extend branches
        val thenBranch = extendSequence(conditional.thn)
        val elseBranch = extendSequence(conditional.els)
        // update conditional
        val instrumented = conditional.copy(
          thn = thenBranch,
          els = elseBranch,
        )(conditional.pos, conditional.info, conditional.errT)
        emit(instrumented)
      case _: ast.Inhale =>
        // TODO: Implement me.
        ???
      case _: ast.Exhale =>
        // TODO: Implement me.
        ???
      case other =>
        emit(other)
    }
}
