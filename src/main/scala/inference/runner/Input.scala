/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.runner

/**
 * Input companion object.
 */
object Input {
  /**
   * Computes an input from the given configuration.
   *
   * @param configuration The configuration.
   * @return The input.
   */
  def apply(configuration: Configuration): Input =
    Input()
}

/**
 * An input to the inference.
 */
case class Input()
