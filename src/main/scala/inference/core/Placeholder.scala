/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.Names
import viper.silver.ast

/**
 * A placeholder for a specification.
 *
 * @param name       The unique name identifying the placeholder.
 * @param parameters The parameters upon which the specification may depend.
 * @param atoms      The atomic predicates that may be used for the specification.
 * @param existing   The existing partial specification.
 */
case class Placeholder(name: String, parameters: Seq[ast.LocalVarDecl], atoms: Seq[ast.Exp], existing: Seq[ast.Exp]) {
  /**
   * The variables corresponding to the parameters.
   */
  lazy val variables: Seq[ast.LocalVar] =
    parameters.map(_.localVar)

  /**
   * Returns whether this specification placeholder corresponds to the recursive predicate.
   *
   * @return True if the specification placeholder corresponds to the recursive predicate.
   */
  def isRecursive: Boolean =
    name == Names.recursive

  /**
   * Returns an instance of the specification placeholder.
   *
   * @return The instance.
   */
  def asInstance: Instance =
    IdentityInstance(this)

  /**
   * Returns a resource corresponding to this specification placeholder.
   *
   * @return The resource.
   */
  def asResource: ast.PredicateAccessPredicate =
    asInstance.asResource

  override def toString: String =
    s"$name(${parameters.map(_.name).mkString(", ")})"
}

/**
 * An instance of a placeholder.
 */
sealed trait Instance {
  /**
   * Returns the placeholder corresponding to the instance.
   *
   * @return The placeholder.
   */
  def placeholder: Placeholder

  /**
   * Returns the name of the placeholder.
   *
   * @return The name.
   */
  def name: String =
    placeholder.name

  /**
   * Returns the arguments to the placeholder.
   *
   * @return The arguments.
   */
  def arguments: Seq[ast.Exp]

  /**
   * Instantiates all occurrences of the parameters in the given expression with their corresponding argument.
   *
   * @param expression The expression to instantiate.
   * @return The instantiated expression.
   */
  def instantiate(expression: ast.Exp): ast.Exp

  /**
   * Instantiates all occurrences of the parameters in the given location access with their corresponding arguments.
   *
   * @param access The location access to instantiate.
   * @return The instantiated location access.
   */
  def instantiate(access: ast.LocationAccess): ast.LocationAccess =
    access match {
      case ast.FieldAccess(receiver, field) =>
        val instantiated = instantiate(receiver)
        ast.FieldAccess(instantiated, field)()
      case ast.PredicateAccess(arguments, name) =>
        val instantiated = arguments.map(instantiate)
        ast.PredicateAccess(instantiated, name)()
    }

  /**
   * Returns a predicate access predicate corresponding to this instance.
   *
   * @return THe predicate access predicate.
   */
  def asResource: ast.PredicateAccessPredicate = {
    val predicate = ast.PredicateAccess(arguments, name)()
    ast.PredicateAccessPredicate(predicate, ast.FullPerm()())()
  }

  /**
   * Returns a copy of the instance with the given arguments.
   *
   * @param arguments The arguments.
   * @return The copy of the instance.
   */
  def apply(arguments: Seq[ast.Exp]): Instance =
    BindingInstance(placeholder, arguments)

  override def toString: String =
    s"$name(${arguments.mkString(", ")})"
}

/**
 * An instance of a placeholder that instantiates all parameters with themselves.
 *
 * @param placeholder The placeholder.
 */
case class IdentityInstance(placeholder: Placeholder) extends Instance {
  override def arguments: Seq[ast.Exp] =
    placeholder.variables

  override def instantiate(expression: ast.Exp): ast.Exp =
    expression
}

/**
 * An instance of a placeholder binding the parameters to some arguments.
 *
 * @param placeholder The placeholder.
 * @param arguments   The arguments.
 */
case class BindingInstance(placeholder: Placeholder, arguments: Seq[ast.Exp]) extends Instance {
  /**
   * The substitution map used for the instantiation of the parameters with their corresponding argument.
   */
  private lazy val bindings: Map[String, ast.Exp] =
    placeholder
      .parameters
      .map(_.name)
      .zip(arguments)
      .toMap

  override def instantiate(expression: ast.Exp): ast.Exp =
    expression.transform {
      case ast.LocalVar(name, _) => bindings(name)
    }
}
