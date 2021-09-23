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
 * A ghost code hint.
 *
 * @param name       The name of the hint.
 * @param argument   The argument for which the hint is meant.
 * @param old        The variable holding the old value of the argument.
 * @param conditions The conditions under which the hint is relevant.
 */
case class Hint(name: String, argument: ast.Exp, old: ast.LocalVar, conditions: Seq[ast.Exp] = Seq.empty) {
  /**
   * Returns whether this is an append hint.
   *
   * @return True if this is an append hint.
   */
  @inline
  def isAppend: Boolean =
    name == Names.append

  /**
   * Returns the hint with the given condition added.
   *
   * @param condition The condition to add.
   * @return The updated hint.
   */
  def withCondition(condition: ast.Exp): Hint =
    copy(conditions = condition +: conditions)
}
