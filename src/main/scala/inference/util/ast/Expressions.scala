/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.ast

import viper.silver.ast

object Expressions {
  /**
   * Returns the conjunction of the given expressions.
   *
   * @param expressions The expressions to conjoin.
   * @return The conjunction.
   */
  def conjoin(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.And(_, _)())
      .getOrElse(ast.TrueLit()())

  /**
   * Returns the disjunction of the given expressions.
   *
   * @param expressions The expressions to disjoin.
   * @return The disjunction.
   */
  def disjoin(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.Or(_, _)())
      .getOrElse(ast.FalseLit()())
}
