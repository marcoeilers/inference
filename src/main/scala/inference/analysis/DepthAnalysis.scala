/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.analysis

import inference.core.Hypothesis
import viper.silver.ast

/**
 * Analysis used to determine the depth up to which predicates should be folded and unfolded.
 */
object DepthAnalysis {
  /**
   * The type of depth functions.
   */
  type Depth = Hypothesis => Int

  /**
   * Analyzes the given statement.
   *
   * @param statement The statement to analyze.
   * @return The depth function.
   */
  def analyze(statement: ast.Stmt): Depth =
    if (true) {
      // TODO: Configuration.
      Function.const(1)
    } else {
      val initial = FunctionState.bottom
      val result = run(statement, initial)
      result.value
    }

  /**
   * Runs the analysis on the given statement with the given initial state.
   *
   * @param statement The statement.
   * @param state     The initial state.
   * @tparam S The type of the state.
   * @return The final state.
   */
  private def run[S <: State[S]](statement: ast.Stmt, state: S): S =
    statement match {
      case ast.Seqn(statements, _) =>
        statements.foldRight(state) { (current, result) => run(current, result) }
      case ast.If(_, left, right) =>
        val leftState = run(left, state)
        val rightState = run(right, state)
        leftState.join(rightState)
      case _: ast.While =>
        sys.error(s"Unexpected loop statement.")
      case other =>
        state.transform(other)
    }

  /**
   * A state of the analysis.
   *
   * @tparam S The type of the state.
   */
  private trait State[S <: State[S]] {
    self: S =>

    /**
     * Returns the depth function.
     *
     * @return The depth function.
     */
    def value: Depth

    /**
     * Returns the join of the two given states.
     *
     * @param left  The left state.
     * @param right The right state.
     * @return The join.
     */
    def join(left: S, right: S): S

    /**
     * Returns the join of this state and the given other state.
     *
     * @param other The other state.
     * @return The join.
     */
    def join(other: S): S =
      join(self, other)

    /**
     * Returns the state obtained from this state by transforming it to capture the effects of the given statement.
     *
     * @param statement The statement.
     * @return The transformed state.
     */
    def transform(statement: ast.Stmt): S
  }

  /**
   * The function state companion object.
   */
  private object FunctionState {
    /**
     * Returns the bottom state.
     */
    val bottom: FunctionState =
      FunctionState(0)

    /**
     * Returns a state holding the constant depth function with the given value.
     *
     * @param value The value.
     * @return The state.
     */
    def apply(value: Int): FunctionState =
      FunctionState(Function.const(value): Depth)

    /**
     * Returns a state holding the given depth function.
     *
     * @param value The depth function.
     * @return The state.
     */
    def apply(value: Depth): FunctionState =
      new FunctionState(value)
  }

  /**
   * A simple state holding a depth function.
   *
   * @param value The depth function.
   */
  private class FunctionState(val value: Depth) extends State[FunctionState] {

    override def join(left: FunctionState, right: FunctionState): FunctionState =
      FunctionState(max(left.value, right.value))

    override def transform(statement: ast.Stmt): FunctionState =
      statement match {
        case _ =>
          // TODO: Implement properly.
          ???
      }
  }

  /**
   * Returns the maximum of the two given depth functions.
   *
   * @param left  The left depth function.
   * @param right The right depth function.
   * @return The maximum.
   */
  private def max(left: Depth, right: Depth): Depth =
    hypothesis => math.max(left(hypothesis), right(hypothesis))
}
