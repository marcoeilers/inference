/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.Names
import viper.silver.ast

/**
 * A folding hint.
 *
 * @param name       The name of the hint.
 * @param argument   The argument for which the hint is meant.
 * @param conditions The conditions under which the hint is relevant.
 */
case class Hint(name: String, argument: ast.Exp, conditions: Seq[ast.Exp] = Seq.empty) {
  /**
   * Returns ture if this is a down hint.
   *
   * @return True if this is a down hint.
   */
  def isDown: Boolean =
    name == Names.downHint

  /**
   * Returns true if this is an up hint.
   *
   * @return True if this is an up hint.
   */
  def isUp: Boolean =
    name == Names.upHint

  /**
   * Returns the hint with the given condition added.
   *
   * @param condition The condition to add.
   * @return The updated hint.
   */
  def withCondition(condition: ast.Exp): Hint =
    copy(conditions = condition +: conditions)
}
