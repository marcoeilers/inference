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
   * The prefix used to generate names for postcondition placeholder.
   */
  val postcondition = "post"

  /**
   * The prefix used to generate names for snapshots.
   */
  val snapshot = "s"

  /**
   * The prefix used to generate names for variables used to save permission values.
   */
  val permission = "p"

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
