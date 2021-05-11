/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Hypothesis
import inference.runner.Input
import viper.silver.ast

/**
 * A query builder.
 */
trait QueryBuilder {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Builds a query that checks the given hypothesis.
   *
   * @param hypothesis The hypothesis to check.
   * @return The query.
   */
  def buildQuery(hypothesis: Hypothesis): Query = {
    val program = instrumentProgram(input.program, hypothesis)
    Query(program)
  }

  /**
   * Instruments the given program.
   *
   * @param program    The program to instrument.
   * @param hypothesis The current hypothesis.
   * @return The instrumented program.
   */
  private def instrumentProgram(program: ast.Program, hypothesis: Hypothesis): ast.Program = {
    val methods = program
      .methods
      .map { method => instrumentMethod(method, hypothesis) }
    program.copy(methods = methods)(program.pos, program.info, program.errT)
  }

  /**
   * Instruments the given method.
   *
   * @param method     The method to instrument.
   * @param hypothesis The current hypothesis.
   * @return The instrumented method.
   */
  private def instrumentMethod(method: ast.Method, hypothesis: Hypothesis): ast.Method = {
    // TODO: Properly implement me.
    method.copy(
      pres = Seq.empty,
      posts = Seq.empty
    )(method.pos, method.info, method.errT)
  }
}
