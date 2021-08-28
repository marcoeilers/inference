/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.util.collections.Collections
import viper.silver.ast

/**
 * An access abstraction.
 */
sealed trait AccessAbstraction {
  /**
   * Returns expressions representing concrete accesses.
   *
   * @return The expressions.
   */
  def expressions: Set[ast.Exp]
}

/**
 * The default implementation of the access abstraction via a set of expressions.
 *
 * @param expressions The expressions.
 */
case class ExpressionSet(expressions: Set[ast.Exp]) extends AccessAbstraction

/**
 * A location access abstraction.
 */
sealed trait LocationAbstraction extends AccessAbstraction {
  /**
   * Returns expressions representing concrete location accesses.
   *
   * @return The expressions.
   */
  def locations: Set[ast.LocationAccess]

  override def expressions: Set[ast.Exp] = {
    // work around sets not being co-variant :(
    locations.map { location => location }
  }
}

/**
 * A field access abstraction.
 *
 * @param receiver The receiver abstraction.
 * @param field    The field.
 */
case class FieldAbstraction(receiver: AccessAbstraction, field: ast.Field) extends LocationAbstraction {
  override lazy val locations: Set[ast.LocationAccess] =
    receiver
      .expressions
      .map { expression => ast.FieldAccess(expression, field)() }
}

/**
 * A predicate access abstraction.
 *
 * @param name      The predicate name.
 * @param arguments The argument abstractions.
 */
case class PredicateAbstraction(name: String, arguments: Seq[AccessAbstraction]) extends LocationAbstraction {
  override lazy val locations: Set[ast.LocationAccess] = {
    val sets = arguments.map(_.expressions)
    Collections
      .product(sets)
      .map { combination => ast.PredicateAccess(combination, name)() }
  }
}
