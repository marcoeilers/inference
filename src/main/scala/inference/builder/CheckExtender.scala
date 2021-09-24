/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.core.Hypothesis
import inference.input.{Check, Cut, Annotation, Instrumented}
import viper.silver.ast

/**
 * A mixin providing methods to build programs from checks.
 *
 * @tparam R The result type.
 */
trait CheckExtender[R] extends Builder with GhostCode {
  /**
   * The current check.
   */
  private var current: Check = _

  override protected def check: Check =
    current

  /**
   * Extends the given check.
   *
   * @param check      The check to extend.
   * @param hypothesis The current hypothesis.
   * @return The extended check.
   */
  protected def extendCheck(check: Check)(implicit hypothesis: Hypothesis): R = {
    // save and update current check
    val outer = current
    current = check
    // process check
    val result = processCheck(check)
    // restore current check and return result
    current = outer
    result
  }

  /**
   * Processes the given check.
   *
   * @param check      The check to process.
   * @param hypothesis The current hypothesis.
   * @return The processed check.
   */
  protected def processCheck(check: Check)(implicit hypothesis: Hypothesis): R

  /**
   * Extends the given sequence.
   *
   * @param sequence   The sequence to extend.
   * @param hypothesis The current hypothesis.
   * @return
   */
  protected def extendSequence(sequence: ast.Seqn)(implicit hypothesis: Hypothesis): ast.Seqn =
    updateScope(sequence)(sequence.ss.foreach(extendStatement))

  /**
   * Extends the given statement.
   *
   * @param statement  The statement to extend.
   * @param hypothesis The current hypothesis.
   */
  protected def extendStatement(statement: ast.Stmt)(implicit hypothesis: Hypothesis): Unit =
    statement match {
      case sequence: ast.Seqn =>
        // extend and emit sequence
        val extended = extendSequence(sequence)
        emit(extended)
      case conditional: ast.If =>
        // extend branches
        val thenExtended = extendSequence(conditional.thn)
        val elseExtended = extendSequence(conditional.els)
        // emit updated conditional
        val extended = conditional.copy(
          thn = thenExtended,
          els = elseExtended
        )(conditional.pos, conditional.info, conditional.errT)
        emit(extended)
      case Instrumented(body, annotations) =>
        processInstrumented(body)(hypothesis, annotations)
      case cut: Cut =>
        processCut(cut)
      case other =>
        emit(other)
    }

  /**
   * Processes the given instrumented statement.
   *
   * @param statement   The instrumented statement.
   * @param hypothesis  The current hypothesis.
   * @param annotations The annotations.
   */
  protected def processInstrumented(statement: ast.Stmt)(implicit hypothesis: Hypothesis, annotations: Seq[Annotation]): Unit

  /**
   * Processes the given cut statement.
   *
   * @param cut        The cut.
   * @param hypothesis The current hypothesis.
   */
  protected def processCut(cut: Cut)(implicit hypothesis: Hypothesis): Unit
}
