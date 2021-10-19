/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.{Hypothesis, Instance}
import inference.teacher.state.Snapshot
import viper.silver.ast

/**
 * A query.
 *
 * @param program    The program representing the query.
 * @param hypothesis The current hypothesis.
 * @param sequence   A sequence containing labels and placeholder instances for all state snapshots along with flags
 *                   whether the snapshot was exhaled or not.
 */
class Query(val program: ast.Program,
            val hypothesis: Hypothesis,
            sequence: Seq[(String, Instance, Boolean)]) {
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
}
