/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Instance
import viper.silver.ast

/**
 * A query.
 *
 * @param program   The program representing the query.
 * @param snapshots A sequence containing labels and placeholder instances for all state snapshots.
 * @param names     The map used to remember the names of permission variables
 */
case class Query(program: ast.Program, snapshots: Seq[(String, Instance, Boolean)], names: Map[String, Map[ast.Exp, String]]) {
  /**
   * Returns the name of the variable that saves the permission value of the given expression in the state snapshot with
   * the given label.
   *
   * @param label      The label of the state snapshot.
   * @param expression The expression.
   * @return The name of the variable.
   */
  def name(label: String, expression: ast.Exp): String =
    names(label)(expression)
}
