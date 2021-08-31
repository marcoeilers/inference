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
 * @param lemmas     The lemma methods.
 */
case class Hypothesis(predicates: Seq[ast.Predicate], lemmas: Seq[ast.Method]) {
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

  /**
   * Returns the inferred specification corresponding to the given placeholder instance.
   *
   * @param instance The placeholder instance.
   * @return The inferred specification.
   */
  def getBody(instance: Instance): ast.Exp = {
    val body = getBody(instance.name)
    instance.instantiate(body)
  }

  override def toString: String =
    if (predicates.isEmpty) "Hypothesis()"
    else {
      val string = predicates
        .map { predicate =>
          val name = predicate.name
          val parameters = predicate.formalArgs.map(_.name).mkString(", ")
          val body = predicate.body.get
          s"  $name($parameters) = $body"
        }
        .mkString("\n")
      s"Hypothesis(\n$string)"
    }
}
