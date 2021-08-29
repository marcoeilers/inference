/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core.sample

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

  /**
   * Returns whether this in an abstraction for the given expression.
   *
   * @param expression The expression.
   * @return True if this is an abstraction for the expression.
   */
  def abstracts(expression: ast.Exp): Boolean

  override def toString: String =
    expressions.mkString("{", ", ", "}")
}

/**
 * The default implementation of the access abstraction via a set of expressions.
 * TODO: Also make an implicit version?
 *
 * @param expressions The expressions.
 */
case class ExplicitSet(expressions: Set[ast.Exp]) extends AccessAbstraction {
  override def abstracts(expression: ast.Exp): Boolean =
    expressions.contains(expression)
}

/**
 * A resource access abstraction.
 */
sealed trait ResourceAbstraction extends AccessAbstraction {
  /**
   * Returns locations representing concrete resource accesses.
   *
   * @return The locations.
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
case class FieldAbstraction(receiver: AccessAbstraction, field: ast.Field) extends ResourceAbstraction {
  override lazy val locations: Set[ast.LocationAccess] =
    receiver
      .expressions
      .map { expression => ast.FieldAccess(expression, field)() }

  override def abstracts(expression: ast.Exp): Boolean =
    expression match {
      case ast.FieldAccess(receiver, `field`) =>
        this
          .receiver
          .abstracts(receiver)
      case _ =>
        false
    }
}

/**
 * A predicate access abstraction.
 *
 * @param name      The predicate name.
 * @param arguments The argument abstractions.
 */
case class PredicateAbstraction(name: String, arguments: Seq[AccessAbstraction]) extends ResourceAbstraction {
  override lazy val locations: Set[ast.LocationAccess] = {
    val sets = arguments.map(_.expressions)
    Collections
      .product(sets)
      .map { combination => ast.PredicateAccess(combination, name)() }
  }

  override def abstracts(expression: ast.Exp): Boolean =
    expression match {
      case ast.PredicateAccess(arguments, `name`) =>
        this
          .arguments
          .zip(arguments)
          .forall { case (abstraction, argument) => abstraction.abstracts(argument) }
      case _ =>
        false
    }
}
