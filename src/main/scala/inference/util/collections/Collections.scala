/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.collections

object Collections {
  /**
   * Returns all unordered pairs that can be formed with the given elements.
   *
   * @param elements The elements.
   * @tparam T The type of the elements.
   * @return The pairs.
   */
  def pairs[T](elements: Iterable[T]): Iterable[(T, T)] =
    if (elements.isEmpty) Seq.empty
    else {
      val first = elements.head
      val rest = elements.tail
      rest.map { other => (first, other) } ++ pairs(rest)
    }
}
