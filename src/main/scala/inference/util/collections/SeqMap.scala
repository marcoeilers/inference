/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.collections

object SeqMap {
  /**
   * Merges the two given maps by concatenating all sequences associated with the same key.
   *
   * @param map1 The first map.
   * @param map2 The second map.
   * @tparam K The key type.
   * @tparam V The value type.
   * @return The merged map.
   */
  @inline
  def marge[K, V](map1: Map[K, Seq[V]], map2: Map[K, Seq[V]]): Map[K, Seq[V]] =
    map2.foldLeft(map1) { case (map, (key, values)) => addAll(map, key, values) }

  /**
   * Adds the given value to the sequence associated with the given key in the given map.
   *
   * @param map   The map.
   * @param key   The key.
   * @param value The value.
   * @tparam K The key type.
   * @tparam V The value type.
   * @return The updated map.
   */
  @inline
  def add[K, V](map: Map[K, Seq[V]], key: K, value: V): Map[K, Seq[V]] =
    map.updated(key, map.get(key).map(_ :+ value).getOrElse(Seq(value)))

  /**
   * Adds the given values to the sequence associated with the given key in the given map.
   *
   * @param map    The map.
   * @param key    The key.
   * @param values The values.
   * @tparam K The key type.
   * @tparam V The value type.
   * @return The updated map.
   */
  @inline
  def addAll[K, V](map: Map[K, Seq[V]], key: K, values: Seq[V]): Map[K, Seq[V]] =
    map.updated(key, map.get(key).map(_ ++ values).getOrElse(values))
}
