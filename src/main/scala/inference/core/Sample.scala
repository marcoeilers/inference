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
sealed trait Sample {
  /**
   * Returns the records mentioned by the sample.
   *
   * @return The records.
   */
  def records: Seq[Record]
}

/**
 * A sample imposing a bound.
 */
sealed trait Bound extends Sample {
  def bound: Int =
    records
      .map(_.delta)
      .sum
}

/**
 * A sample imposing a strict lower bound.
 *
 * @param records The records.
 */
case class LowerBound(records: Seq[Record]) extends Bound {
  override def toString: String =
    s"$bound < ${records.mkString(" + ")}"
}

/**
 * A sample imposing an upper bound.
 *
 * @param record The record.
 */
case class UpperBound(record: Record) extends Bound {
  override def records: Seq[Record] =
    Seq(record)

  override def toString: String =
    s"$record <= 1"
}

/**
 * An implication sample.
 *
 * @param left  The left-hand side of the implication.
 * @param right The right-hand side of the implication.
 */
case class Implication(left: Record, right: LowerBound) extends Sample {
  override def records: Seq[Record] =
    left +: right.records

  override def toString: String =
    s"$left => $right"
}

/**
 * A record representing a data point.
 */
sealed trait Record {
  /**
   * Returns the specification placeholder corresponding to this data point.
   *
   * @return The specification placeholder.
   */
  def placeholder: Placeholder

  /**
   * Returns the state abstraction.
   *
   * @return The state abstraction.
   */
  def state: StateAbstraction

  /**
   * Returns the resource abstraction.
   *
   * @return The resource abstraction
   */
  def resource: ResourceAbstraction

  /**
   * Returns the permission difference.
   *
   * @return The permission difference.
   */
  def delta: Int
}

/**
 * A record representing a data point corresponding to an inhaled state snapshot.
 *
 * @param placeholder See [[Record.placeholder]].
 * @param state       See [[Record.state]].
 * @param resource    See [[Record.resource]].
 * @param amount      The permission amount (i.e. absolute value of permission difference).
 */
case class InhaledRecord(placeholder: Placeholder, state: StateAbstraction, resource: ResourceAbstraction, amount: Int) extends Record {
  override def delta: Int =
    amount

  override def toString: String =
    s"inhale@${placeholder.name}: $state -> $resource"
}

/**
 * A record representing a data point corresponding to an exhaled state snapshot.
 *
 * @param placeholder See [[Record.placeholder]].
 * @param state       See [[Record.state]].
 * @param resource    See [[Record.resource]].
 * @param amount      The permission amount (i.e. absolute value of permission difference).
 */
case class ExhaledRecord(placeholder: Placeholder, state: StateAbstraction, resource: ResourceAbstraction, amount: Int) extends Record {
  override def delta: Int =
    -amount

  override def toString: String =
    s"exhale@${placeholder.name}: $state -> $resource"
}

/**
 * A state abstraction.
 */
sealed trait StateAbstraction {
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
case class SnapshotAbstraction(snapshot: Snapshot) extends StateAbstraction {
  override def evaluate(atom: ast.Exp): Option[Boolean] = {
    // TODO: Can the value ever be unknown?
    val actual = snapshot.instance.instantiate(atom)
    val value = snapshot.state.evaluateBoolean(actual)
    Some(value)
  }

  override def toString: String = {
    val partitions = snapshot.partitions
    if (partitions.isEmpty) "true"
    else partitions
      .map(_.mkString("="))
      .mkString("≠")
  }
}

/**
 * A resource abstraction.
 */
sealed trait ResourceAbstraction {
  /**
   * Returns the set of locations that can be used to represent the offending resource.
   *
   * @return The set of locations.
   */
  def locations: Set[ast.LocationAccess]
}

/**
 * A resource abstracted by a set of locations that can be used to represent the offending resource.
 *
 * @param locations The locations.
 */
case class SetAbstraction(locations: Set[ast.LocationAccess]) extends ResourceAbstraction {
  override def toString: String =
    locations.mkString("{", ",", "}")
}
