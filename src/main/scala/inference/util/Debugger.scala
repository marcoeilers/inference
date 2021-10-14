/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util

object Debugger {
  /**
   * Dummy method that can be called in order to inspect values in places where the Scala debugger otherwise refuses to
   * break.
   *
   * @param value The value to inspect.
   * @tparam T The type of the value.
   * @return The value.
   */
  def break[T](value: T): T =
    value
}
