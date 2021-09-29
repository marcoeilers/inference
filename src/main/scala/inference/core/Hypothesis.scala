/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.Names
import inference.util.ast.Expressions
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
  private lazy val predicateMap =
    predicates
      .map { predicate => predicate.name -> predicate }
      .toMap

  /**
   * The map from names to lemma methods.
   */
  private lazy val lemmaMap =
    lemmas
      .map { lemma => lemma.name -> lemma }
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
    predicateMap
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

  /**
   * Returns the lemma method with the given name.
   *
   * @param name The name of the lemma.
   * @return The lemma method.
   */
  def getLemma(name: String): ast.Method =
    lemmaMap.get(name) match {
      case Some(method) => method
      case _ => sys.error(s"Lemma method not defined: $name")
    }

  /**
   * Returns the precondition corresponding to the given lemma instance.
   *
   * @param instance The lemma instance.
   * @return The precondition.
   */
  def getLemmaPrecondition(instance: Instance): ast.Exp = {
    val lemma = getLemma(instance.name)
    val precondition = Expressions.makeAnd(lemma.pres)
    instance.instantiate(precondition)
  }

  /**
   * Returns the postcondition corresponding to the given lemma instance.
   *
   * @param instance The lemma instance.
   * @return The postcondition.
   */
  def getLemmaPostcondition(instance: Instance): ast.Exp = {
    val lemma = getLemma(instance.name)
    val postcondition = Expressions.makeAnd(lemma.posts)
    instance.instantiate(postcondition)
  }

  /**
   * Returns a list containing all recursive links appearing in the given recursive predicate instance.
   *
   * @param instance The recursive predicate instance.
   * @return The list of links.
   */
  def getLinks(instance: Instance): Seq[ast.FieldAccess] = {
    assert(instance.isRecursive)
    val body = getBody(instance)
    getLinks(body)
  }

  /**
   * Returns a list containing all recursive links appearing in the given expression.
   *
   * @param expression The expression.
   * @return The list of links.
   */
  private def getLinks(expression: ast.Exp): Seq[ast.FieldAccess] =
    expression match {
      case ast.And(left, right) =>
        getLinks(left) ++ getLinks(right)
      case ast.Implies(_, right) =>
        getLinks(right)
      case ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _) =>
        assert(Names.isRecursive(name))
        arguments.head match {
          case access: ast.FieldAccess => Seq(access)
          case other => sys.error(s"Unexpected link: $other")
        }
      case _ =>
        Seq.empty
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
