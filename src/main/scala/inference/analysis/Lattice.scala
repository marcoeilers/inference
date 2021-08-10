/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.analysis

/**
 * An element of a lattice.
 *
 * @tparam T The type of the lattice elements.
 */
trait Lattice[T <: Lattice[T]] {
  self: T =>
}

/**
 * A join mixin for lattices.
 *
 * @tparam T The typ of the lattice elements.
 */
trait Join[T <: Lattice[T]] extends Lattice[T] {
  self: T =>

  /**
   * Returns the join of the two given elements.
   *
   * @param left  The left element.
   * @param right The right element.
   * @return The join.
   */
  def join(left: T, right: T): T

  /**
   * Returns the join of this and the given other element.
   *
   * @param other The other element.
   * @return The join.
   */
  def join(other: T): T =
    join(self, other)
}

/**
 * A meet mixin for lattices.
 *
 * @tparam T The type of the lattice elements.
 */
trait Meet[T <: Lattice[T]] extends Lattice[T] {
  self: T =>

  /**
   * Returns the meet of the two given elements.
   *
   * @param left  The left element.
   * @param right The right element.
   * @return The meet.
   */
  def meet(left: T, right: T): T

  /**
   * Returns the meet of this and the given other element.
   *
   * @param other The other element.
   * @return The meet.
   */
  def meet(other: T): T =
    meet(self, other)
}

/**
 * Top mixin for lattices.
 *
 * @tparam T The type of the lattice elements.
 */
trait Top[T <: Lattice[T]] extends Lattice[T] {
  self: T =>

  /**
   * Returns the top element of the lattice.
   *
   * @return The top element.
   */
  def top: T
}

/**
 * Bottom mixin for lattices.
 *
 * @tparam T The type of the lattice elements.
 */
trait Bottom[T <: Lattice[T]] extends Lattice[T] {
  self: T =>

  /**
   * Returns the bottom element of the lattice.
   *
   * @return The bottom element.
   */
  def bottom: T
}
