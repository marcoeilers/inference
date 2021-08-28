/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core.sample

import inference.teacher.state.{Snapshot, StateEvaluator}
import viper.silver.ast

/**
 * A state abstracted by some snapshot.
 *
 * @param snapshot The snapshot.
 */
case class StateAbstraction(snapshot: Snapshot) {
  /**
   * Returns the state.
   *
   * @return The state.
   */
  def state: StateEvaluator =
    snapshot.state

  /**
   * Evaluates the given atomic predicate in the abstract state.
   *
   * @param atom The atomic predicate to evaluate.
   * @return The predicate value.
   */
  def evaluate(atom: ast.Exp): Option[Boolean] = {
    // TODO: Can the value ever be unknown?
    val actual = snapshot.instance.instantiate(atom)
    val value = state.evaluateBoolean(actual)
    Some(value)
  }

  /**
   * Evaluates the given atomic predicates in the abstract state.
   *
   * @param atoms The atomic predicates to evaluate.
   * @return The predicate values.
   */
  def evaluate(atoms: Seq[ast.Exp]): Seq[Option[Boolean]] =
    atoms.map(evaluate)

  override def toString: String = {
    val partitions = snapshot.partitions
    if (partitions.isEmpty) "true"
    else partitions
      .map(_.mkString("="))
      .mkString("≠")
  }
}
