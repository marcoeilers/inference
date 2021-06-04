/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import viper.silver.ast

/**
 * A hypothesis.
 *
 * @param predicates The predicates.
 */
case class Hypothesis(predicates: Seq[ast.Predicate]) {
  /**
   * The map from names to predicates.
   */
  private lazy val map =
    predicates
      .map { predicate => predicate.name -> predicate }
      .toMap

  /**
   * Returns the inferred specification predicate corresponding to the given placeholder.
   *
   * @param placeholder The placeholder.
   * @return The predicate.
   */
  def getPredicate(placeholder: Placeholder): ast.Predicate = {
    val name = placeholder.name
    val parameters = placeholder.parameters
    val body = getBody(name)
    ast.Predicate(name, parameters, Some(body))()
  }

  /**
   * Returns the inferred specification corresponding to the placeholder with the given name.
   *
   * @param name The name of the placeholder.
   * @return The inferred specification.
   */
  def getBody(name: String): ast.Exp =
    map
      .get(name)
      .flatMap(_.body)
      .getOrElse(ast.TrueLit()())

  def getBody(instance: Instance): ast.Exp = {
    val expression = getBody(instance.name)
    instance.instantiate(expression)
  }
}
