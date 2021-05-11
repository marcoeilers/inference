/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

/**
 * A namespace used to generate unique identifiers.
 *
 * @param map A map associating identifiers with version numbers.
 */
class Namespace(private var map: Map[String, Int] = Map.empty) {
  /**
   * Returns a unique identifier.
   *
   * @param name    The base name of the identifier.
   * @param version The optional version number.
   * @return A unique identifier.
   */
  def uniqueIdentifier(name: String, version: Option[Int] = Some(0)): String =
    if (version.isDefined || map.contains(name)) {
      var current = math.max(version.getOrElse(0), map.getOrElse(name, 0))
      while (map.contains(s"${name}_$current")) current = current + 1
      s"${name}_$current"
    } else {
      map = map.updated(name, 0)
      name
    }

  /**
   * Returns a copy of the namespace.
   *
   * @return The copied namespace.
   */
  def copy(): Namespace =
    new Namespace(map)
}
