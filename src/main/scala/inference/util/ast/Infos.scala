/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.ast

import inference.core.Instance
import viper.silver.ast
import viper.silver.ast.LocationAccess

/**
 * Utility object for infos.
 */
object Infos {
  /**
   * Returns whether the given variable is tagged as saved.
   *
   * @param variable The variable.
   * @return True if the variable is tagged as saved.
   */
  def isSaved(variable: ast.LocalVar): Boolean =
    variable
      .info
      .getUniqueInfo[SavedInfo.type]
      .isDefined
}

/**
 * An info carrying a value of some type.
 *
 * @tparam T The type of the value carried by the info.
 */
trait InferenceInfo[+T] extends ast.Info {
  /**
   * Returns the value carried by the info.
   *
   * @return The info.
   */
  def value: T

  override def comment: Seq[String] =
    Seq.empty

  override def isCached: Boolean =
    true
}


/**
 * An info carrying an instance.
 *
 * @param instance The instance.
 */
case class InstanceInfo(instance: Instance) extends InferenceInfo[Instance] {
  override def value: Instance =
    instance
}

/**
 * An info carrying a location.
 *
 * @param location The location
 */
case class LocationInfo(location: ast.LocationAccess) extends InferenceInfo[ast.LocationAccess] {
  override def value: LocationAccess =
    location
}

/**
 * An info used to tag saved variables.
 */
case object SavedInfo extends InferenceInfo[Unit] {
  override def value: Unit = {}
}

/**
 * A mixin that enables comments.
 */
trait Comment extends InferenceInfo[Any] {
  override def comment: Seq[String] =
    Seq(value.toString)
}
