/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.collections

object SetMap {
  /**
   * Merges the two given maps by unifying all sets associated with the same key.
   *
   * @param map1 The first map.
   * @param map2 The second map.
   * @tparam K The key type.
   * @tparam V The value type.
   * @return The merged map.
   */
  @inline
  def union[K, V](map1: Map[K, Set[V]], map2: Map[K, Set[V]]): Map[K, Set[V]] =
    map2.foldLeft(map1) { case (map, (key, value)) => addAll(map, key, value) }

  /**
   * Adds the given value to the set associated with the given key in the given map.
   *
   * @param map   The map.
   * @param key   The key.
   * @param value The value.
   * @tparam K The key type.
   * @tparam V The value type.
   * @return The updated map.
   */
  @inline
  def add[K, V](map: Map[K, Set[V]], key: K, value: V): Map[K, Set[V]] =
    map.updated(key, map.get(key).map(_ + value).getOrElse(Set(value)))

  /**
   * Adds the given values to the set associated with the given key in the given map.
   *
   * @param map    The map.
   * @param key    THe key.
   * @param values The values.
   * @tparam K The key type.
   * @tparam V THe value type.
   * @return The updated map.
   */
  @inline
  def addAll[K, V](map: Map[K, Set[V]], key: K, values: Set[V]): Map[K, Set[V]] =
    map.updated(key, map.get(key).map(_ ++ values).getOrElse(values))
}
