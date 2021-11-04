/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

import viper.silver.ast

/**
 * Namespace companion object.
 */
object Namespace {
  /**
   * Creates a namespaces corresponding to the given program.
   *
   * @param program The program.
   * @return The namespace.
   */
  def apply(program: ast.Program): Namespace = {
    val namespace = new Namespace()
    namespace.addIdentifiers(program.fields.map(_.name))
    namespace.addIdentifiers(program.functions.map(_.name))
    namespace.addIdentifiers(program.methods.map(_.name))
    namespace
  }
}

/**
 * A namespace used to generate unique identifiers.
 *
 * @param map A map associating identifiers with version numbers.
 */
class Namespace(private var map: Map[String, Int] = Map.empty, template: String = "%s_%d") {
  /**
   * Adds the identifier with the given name to the namespace.
   *
   * @param name The name of the identifier.
   */
  def addIdentifier(name: String): Unit = {
    assert(!map.contains(name))
    map = map.updated(name, 0)
  }

  /**
   * Adds the identifiers with the given names to the namespace.
   *
   * @param names The names of the identifiers.
   */
  def addIdentifiers(names: Seq[String]): Unit =
    names.foreach(addIdentifier)

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
      var identifier = template.format(name, current)
      while (map.contains(identifier)) {
        current = current + 1
        identifier = template.format(name, current)
      }
      map = map.updated(name, current + 1)
      identifier
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
    new Namespace(map, template)
}
