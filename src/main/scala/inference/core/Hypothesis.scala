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
    val body = {
      val instance = placeholder.asInstance
      val specifications = getSpecifications(instance)
      Expressions.makeAnd(specifications)
    }
    ast.Predicate(name, parameters, Some(body))()
  }

  /**
   * Returns the specifications corresponding to the given placeholder instance.
   *
   * @param instance The instance.
   * @return The specifications.
   */
  def getSpecifications(instance: Instance): Seq[ast.Exp] = {
    val inferred = getInferred(instance)
    val existing = instance.existing

    /**
     * Helper function that collects guarded resources provided in the given expression.
     *
     * @param expression The expression.
     * @param guards     The current guards.
     * @return The guarded resources.
     */
    def collectResources(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty): Seq[(ast.PredicateAccessPredicate, Seq[ast.Exp])] =
      expression match {
        case ast.And(left, right) =>
          collectResources(left, guards) ++ collectResources(right, guards)
        case ast.Implies(left, right) =>
          val updatedGuards = guards :+ left
          collectResources(right, updatedGuards)
        case resource: ast.PredicateAccessPredicate =>
          Seq((resource, guards))
        case _ =>
          Seq.empty
      }

    /**
     * Helper function that unfolds the given guarded resources in the given expression.
     *
     * @param resources  The guarded resources.
     * @param expression The expression.
     * @return The resulting expression.
     */
    def unfoldResources(resources: Seq[(ast.PredicateAccessPredicate, Seq[ast.Exp])], expression: ast.Exp): ast.Exp =
      resources match {
        case (predicate, guards) +: rest =>
          // compute body and unfolded body
          val body = unfoldResources(rest, expression)
          val unfolded = ast.Unfolding(predicate, body)()
          // conditionally unfold
          if (guards.isEmpty) {
            unfolded
          } else {
            val condition = Expressions.makeAnd(guards)
            ast.CondExp(condition, unfolded, body)()
          }
        case _ =>
          expression
      }

    lazy val resources = collectResources(inferred)
    val unfolded = existing.map { specification =>
      // check whether existing specification contains a field access
      val hasField = specification.exists {
        case _: ast.FieldAccess => true
        case _ => false
      }
      // only unfold resources if a field access is present
      if (hasField) {
        unfoldResources(resources, specification)
      } else {
        specification
      }
    }

    inferred +: unfolded
  }

  /**
   * Returns the inferred specification corresponding to the given placeholder instance.
   *
   * @param instance The instance.
   * @return The inferred specification.
   */
  def getInferred(instance: Instance): ast.Exp = {
    val inferred = getInferred(instance.name)
    instance.instantiate(inferred)
  }

  /**
   * Returns the inferred specification corresponding to the placeholder with the given name.
   *
   * @param name The name of the placeholder.
   * @return The inferred specification.
   */
  private def getInferred(name: String): ast.Exp =
    predicateMap
      .get(name)
      .flatMap(_.body)
      .getOrElse(ast.TrueLit()())

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
    val body = getInferred(instance)
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
