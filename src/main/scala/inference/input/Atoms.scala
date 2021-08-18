/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.util.collections.Collections
import viper.silver.ast

/**
 * A mixin used to generate atomic predicates.
 */
trait Atoms {
  /**
   * Computes atoms corresponding to the given parameters.
   *
   * @param parameters The parameters.
   * @return The atoms.
   */
  protected def atomsFromParameters(parameters: Seq[ast.LocalVarDecl]): Seq[ast.Exp] = {
    // get variables corresponding to reference-typed parameters
    val references = parameters
      .filter(_.isSubtype(ast.Ref))
      .map(_.localVar)
    // compute atoms
    atomsFromExpressions(references)
  }

  /**
   * Computes atoms corresponding to the given expressions.
   *
   * @param expressions The expressions.
   * @return The atoms.
   */
  protected def atomsFromExpressions(expressions: Seq[ast.Exp]): Seq[ast.Exp] = {
    // TODO: Use null!
    val withNull = ast.NullLit()() +: expressions
    Collections
      .pairs(expressions)
      .map { case (first, second) => ast.NeCmp(first, second)() }
      .toSeq
  }
}
