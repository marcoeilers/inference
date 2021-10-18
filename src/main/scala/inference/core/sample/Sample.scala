/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core.sample

import inference.core.Placeholder

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
 * Lower bound companion object.
 */
object LowerBound {
  /**
   * Returns a lower bound sample with the given record.
   *
   * @param record The record.
   * @return The lower bound.
   */
  def apply(record: Record): LowerBound =
    LowerBound(Seq(record))
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
case class Implication(left: LowerBound, right: LowerBound) extends Sample {
  override def records: Seq[Record] =
    left.records ++ right.records

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
    s"inhale@${placeholder.name}: $state -> $resource [$amount]"
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
    s"exhale@${placeholder.name}: $state -> $resource [$amount]"
}
