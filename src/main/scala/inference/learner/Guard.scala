/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.sample.{Record, ResourceAbstraction, StateAbstraction}
import inference.util.collections.SeqMap
import viper.silver.ast

/**
 * A helper object used to compute effective guards.
 */
object Guards {
  /**
   * Type shortcut for effective guards.
   *
   * An effective guard is represented by a map where the keys are syntactic expressions that may be used to refer to
   * the offending location. The values are sequences of options where each option is a sequence of guards that
   * conjunctively guard the syntactic location.
   */
  type Effective = Map[ast.Exp, Seq[Seq[Guard]]]

  /**
   * Computes the effective guard corresponding to the given record under consideration of the implicitly passed
   * predicate templates.
   *
   * @param record    The record.
   * @param templates The predicate templates.
   * @return The effective guard.
   */
  def effective(record: Record)(implicit templates: Map[String, PredicateTemplate]): Effective = {
    implicit val resource: ResourceAbstraction = record.resource
    implicit val state: StateAbstraction = record.state
    // get and process template
    val name = record.placeholder.name
    templates
      .get(name)
      .map { template => processTemplate(template) }
      .getOrElse(Map.empty)
  }

  /**
   * Computes the effective guard corresponding to the given predicate template.
   *
   * @param template  The predicate template.
   * @param depth     The current depth.
   * @param view      The current view.
   * @param guards    The current guards.
   * @param resource  The resource for which to compute the effective guard.
   * @param state     The state.
   * @param templates The map containing all predicate templates.
   * @return The effective guard.
   */
  private def processTemplate(template: PredicateTemplate,
                              depth: Int = 0,
                              view: View = View.empty,
                              guards: Seq[Guard] = Seq.empty)
                             (implicit resource: ResourceAbstraction,
                              state: StateAbstraction,
                              templates: Map[String, PredicateTemplate]): Effective =
    if (depth < 3) {
      // get and adapt atoms
      val atoms = template
        .placeholder
        .atoms
        .map(view.adapt)
      // get and process template body
      val expression = template.body
      processExpression(expression, depth, view, guards, atoms)
    } else Map.empty

  /**
   * Computes the effective guard corresponding to the given template expression.
   *
   * @param expression The template expression.
   * @param depth      The current depth.
   * @param view       The current view.
   * @param guards     The current guards.
   * @param atoms      The current atoms.
   * @param resource   The resource for which to compute the effective guard.
   * @param state      The state.
   * @param templates  The map containing all predicate templates.
   * @return The effective guard.
   */
  private def processExpression(expression: TemplateExpression,
                                depth: Int,
                                view: View,
                                guards: Seq[Guard],
                                atoms: Seq[ast.Exp])
                               (implicit resource: ResourceAbstraction,
                                state: StateAbstraction,
                                templates: Map[String, PredicateTemplate]): Effective =
    expression match {
      case Wrapped(wrapped) =>
        wrapped match {
          case ast.FieldAccessPredicate(access, _) =>
            // adapt field access
            val adapted = view.adaptFieldAccess(access)
            // consider current guards if resource abstraction contains field access
            if (resource.abstracts(adapted)) Map(adapted -> Seq(guards))
            else Map.empty
          case ast.PredicateAccessPredicate(access, _) =>
            // adapt predicate access
            val adapted = view.adaptPredicateAccess(access)
            // process nested template expressions
            val nested = {
              val template = templates(adapted.predicateName)
              val view = View.create(template, adapted.args)
              processTemplate(template, depth + 1, view, guards)
            }
            // consider current guards if resource is equal to predicate access
            if (resource.abstracts(adapted)) SeqMap.add(nested, adapted, guards)
            else nested
          case other =>
            sys.error(s"Unexpected wrapped expression in template: $other")
        }
      case Conjunction(conjuncts) =>
        conjuncts
          .map { conjunct => processExpression(conjunct, depth, view, guards, atoms) }
          .reduceOption(SeqMap.marge[ast.Exp, Seq[Guard]])
          .getOrElse(Map.empty)
      case Guarded(guardId, body) =>
        val guard = ResourceGuard(guardId, atoms)
        val updatedGuards = guards :+ guard
        processExpression(body, depth, view, updatedGuards, atoms)
      case Truncated(condition, body) =>
        val guard = TruncationGuard(view.adapt(condition))
        val updatedGuards = guards :+ guard
        processExpression(body, depth, view, updatedGuards, atoms)
    }

  /**
   * The companion object for views.
   */
  object View {
    /**
     * Returns the empty view that does not associated any variable with any expression.
     *
     * @return The empty view.
     */
    def empty: View =
      View(Map.empty)

    /**
     * Creates the view for the given template with the given arguments.
     *
     * @param template  The template.
     * @param arguments The arguments.
     * @return The view.
     */
    def create(template: Template, arguments: Seq[ast.Exp]): View = {
      val map = template
        .placeholder
        .parameters
        .map(_.name)
        .zip(arguments)
        .toMap
      View(map)
    }
  }

  /**
   * A view mapping variable names ot the expressions with which they are instantiated.
   *
   * @param map The map.
   */
  case class View(map: Map[String, ast.Exp]) {
    /**
     * Returns whether the view is empty.
     *
     * @return True if the view is empty.
     */
    def isEmpty: Boolean =
      map.isEmpty

    /**
     * Adapts the expression according to the view.
     *
     * @param expression The expression to adapt.
     * @return The adapted expression.
     */
    def adapt(expression: ast.Exp): ast.Exp =
      if (isEmpty) expression
      else expression.transform {
        case variable@ast.LocalVar(name, _) =>
          map.getOrElse(name, variable)
      }

    /**
     * Adapts the given field access according to the view.
     *
     * @param access The field access to adapt.
     * @return The adapted field access.
     */
    def adaptFieldAccess(access: ast.FieldAccess): ast.FieldAccess = {
      val receiver = adapt(access.rcv)
      val field = access.field
      ast.FieldAccess(receiver, field)()
    }

    /**
     * Adapts the given predicate access according to the view.
     *
     * @param access The predicate access to adapt.
     * @return The adapted predicate access.
     */
    def adaptPredicateAccess(access: ast.PredicateAccess): ast.PredicateAccess = {
      val name = access.predicateName
      val arguments = access.args.map(adapt)
      ast.PredicateAccess(arguments, name)()
    }

    /**
     * Returns the updated view where the variable with the given name is associated with the given expression.
     *
     * @param name       The name of the variable.
     * @param expression The expression.
     * @return The updated view.
     */
    def updated(name: String, expression: ast.Exp): View =
      View(map.updated(name, expression))
  }
}

/**
 * The super trait for all guards (used to represent effective guards).
 */
sealed trait Guard

/**
 * A resource guard.
 *
 * @param guardId The guard id.
 * @param atoms   The atomic predicates.
 */
case class ResourceGuard(guardId: Int, atoms: Seq[ast.Exp]) extends Guard {
  override def toString: String =
    s"phi_$guardId[${atoms.mkString(", ")}]"
}

/**
 * A choice guard.
 *
 * @param choiceId The choice id.
 * @param index    The index of the selected choice.
 */
case class ChoiceGuard(choiceId: Int, index: Int) extends Guard {
  override def toString: String =
    s"c_$choiceId=$index"
}

/**
 * A truncation guard.
 *
 * @param condition The truncation condition.
 */
case class TruncationGuard(condition: ast.Exp) extends Guard {
  override def toString: String =
    condition.toString()
}
