/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

object Names {
  /**
   * The prefix used to generate names for precondition placeholders.
   */
  val precondition = "pre"

  /**
   * The prefix used to generate names for postcondition placeholders.
   */
  val postcondition = "post"

  /**
   * The prefix used to generate names for invariant placeholders.
   */
  val invariant = "inv"

  /**
   * The prefix used to generate names for snapshots.
   */
  val snapshot = "s"

  /**
   * The prefix used to generate names for auxiliary variables.
   */
  val auxiliary = "t"

  /**
   * The name used for the recursive predicate.
   */
  val recursive = "P"

  /**
   * The name of the down hint.
   */
  val downHint = "__down__"

  /**
   * The name of the up hint.
   */
  val upHint = "__up__"

  /**
   * All hint names.
   */
  val hints = Seq(
    downHint,
    upHint
  )

  /**
   * Returns whether the given name corresponds to a hint.
   *
   * @param name The name to check.
   * @return True if the name corresponds to a hint.
   */
  def isHint(name: String): Boolean =
    hints.contains(name)

  /**
   * Returns the variable name used for the activation of a clause.
   *
   * @param guardId     The guard id.
   * @param clauseIndex The clause index.
   * @return The variable name.
   */
  @inline
  def clauseActivation(guardId: Int, clauseIndex: Int): String =
    s"x-$guardId-$clauseIndex"

  /**
   * Returns the variable name used for the activation of a literal.
   *
   * @param guardId      The guard id.
   * @param clauseIndex  The clause index.
   * @param literalIndex The literal index.
   * @return The variable name.
   */
  @inline
  def literalActivation(guardId: Int, clauseIndex: Int, literalIndex: Int): String =
    s"y-$guardId-$clauseIndex-$literalIndex"

  /**
   * Returns the variable name used for the sign of a literal.
   *
   * @param guardId      The guard id.
   * @param clauseIndex  The clause index.
   * @param literalIndex The literal index.
   * @return The variable name.
   */
  @inline
  def literalSign(guardId: Int, clauseIndex: Int, literalIndex: Int): String =
    s"s-$guardId-$clauseIndex-$literalIndex"
}
