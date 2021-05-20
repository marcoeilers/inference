/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.teacher.state.Snapshot
import viper.silver.ast

/**
 * A sample.
 */
sealed trait Sample

/**
 * A lower bound sample.
 *
 * @param record The record.
 */
case class LowerBound(record: Record) extends Sample

/**
 * A record representing a data point.
 *
 * @param placeholder The specification placeholder corresponding to this record.
 * @param abstraction The state abstraction.
 * @param locations   The set of locations that can be used to represent the resource in question.
 */
case class Record(placeholder: Placeholder, abstraction: Abstraction, locations: Set[ast.LocationAccess])

/**
 * A state abstraction.
 */
trait Abstraction {
  /**
   * Evaluates the given atomic predicate in the abstract state.
   *
   * @param atom The atomic predicate to evaluate.
   * @return The predicate value.
   */
  def evaluate(atom: ast.Exp): Option[Boolean]

  /**
   * Evaluates the given atomic predicates in the abstract state.
   *
   * @param atoms The atomic predicates to evaluate.
   * @return The predicate values.
   */
  def evaluate(atoms: Seq[ast.Exp]): Seq[Option[Boolean]] =
    atoms.map(evaluate)
}

/**
 * A state abstracted by some snapshot.
 *
 * @param snapshot The snapshot.
 */
case class SnapshotAbstraction(snapshot: Snapshot) extends Abstraction {
  override def evaluate(atom: ast.Exp): Option[Boolean] = {
    // TODO: Can the value ever be unknown?
    val actual = snapshot.instance.instantiate(atom)
    val value = snapshot.state.evaluateBoolean(actual)
    Some(value)
  }

  override def toString: String =
    snapshot
      .partitions.map(_.mkString("{", ",", "}"))
      .mkString("{", ",", "}")
}
