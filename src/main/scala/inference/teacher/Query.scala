/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Instance
import viper.silver.ast

/**
 * A query.
 *
 * @param program  The program representing the query.
 * @param sequence A sequence containing labels and placeholder instances for all state snapshots along with flags
 *                 whether the snapshot was exhaled or not.
 * @param names    The map used to remember the names of permission variables
 */
class Query(val program: ast.Program, sequence: Seq[(String, Instance, Boolean)], names: Map[String, Map[ast.Exp, String]]) {
  /**
   * The set containing the names of all exhaled state snapshots.
   */
  private val exhaled: Set[String] =
    sequence
      .filter(_._3)
      .map(_._1)
      .toSet

  /**
   * Returns a sequence containing labels and placeholder instances for all state snapshots.
   *
   * @return The sequence.
   */
  def snapshots: Seq[(String, Instance)] =
    sequence.map { case (label, instance, _) => (label, instance) }

  /**
   * Returns whether the given label corresponds to an exhaled state snapshot.
   *
   * @param name The label of the snapshot.
   * @return True if the label corresponds to an exhaled state snapshot.
   */
  def isExhaled(name: String): Boolean =
    exhaled.contains(name)

  /**
   * Returns the name of the variable that saves the permission value of the given expression in the state snapshot with
   * the given label.
   *
   * @param label      The label of the state snapshot.
   * @param expression The expression.
   * @return The name of the variable.
   */
  def name(label: String, expression: ast.Exp): String =
    names(label)(expression)
}
