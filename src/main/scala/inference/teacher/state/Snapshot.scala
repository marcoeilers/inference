/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import inference.core.{Instance, Placeholder}
import inference.util.collections.SetMap
import viper.silver.ast

/**
 * A state snapshot.
 *
 * @param instance The placeholder instance corresponding to the state snapshot.
 * @param state    The state evaluator.
 * @param exhaled  The flag indicating whether the snapshot was exhaled or not.
 */
case class Snapshot(instance: Instance, state: StateEvaluator, exhaled: Boolean) {
  /**
   * Lazily computed reachability map used to adapt expressions.
   */
  private[state] lazy val reachability = {
    /**
     * Helper method that recursively updates the reachability map.
     *
     * @param current The current reachability map.
     * @param steps   The number of steps.
     * @return The final reachability map.
     */
    def recurse(current: Map[String, Set[ast.Exp]], steps: Int): Map[String, Set[ast.Exp]] =
      if (steps == 0) current
      else {
        // compute next step of reachability
        val next = current.foldLeft(Map.empty[String, Set[ast.Exp]]) {
          case (map1, (node, expressions)) => state
            .heap
            .getOrElse(node, Map.empty)
            .foldLeft(map1) {
              case (map2, (name, value)) =>
                val extended = expressions.map { expression =>
                  val field = ast.Field(name, ast.Ref)()
                  ast.FieldAccess(expression, field)(): ast.Exp
                }
                SetMap.addAll(map2, value, extended)
            }
        }
        // recurse and combine result
        val future = recurse(next, steps - 1)
        SetMap.union(current, future)
      }

    // initial reachability map
    val parameters = instance.placeholder.variables
    val initial = instance
      .arguments
      .zip(parameters)
      .foldLeft(Map.empty[String, Set[ast.Exp]]) {
        case (result, (argument, parameter)) =>
          if (argument.typ == ast.Ref) {
            val value = state.evaluateReference(argument)
            SetMap.add(result, value, parameter)
          } else result
      }

    // recursively follow fields
    // TODO: Number of steps
    recurse(initial, steps = 3)
  }

  /**
   * Returns the label of the state snapshot.
   *
   * @return The label.
   */
  def label: String =
    state.label.get

  /**
   * Returns the placeholder corresponding to the snapshot.
   *
   * @return The placeholder.
   */
  def placeholder: Placeholder =
    instance.placeholder

  /**
   * Returns the partitioned heap, i.e., sets of equal expressions.
   *
   * @return The partitions.
   */
  def partitions: Iterable[Set[ast.Exp]] =
    reachability.map { case (_, set) => set }
}

/**
 * An adaptor used to adapt expressions from one state to another.
 *
 * @param source The source state.
 * @param target The target state.
 */
case class Adaptor(source: StateEvaluator, target: Snapshot) {
  /**
   * Adapts the given location.
   *
   * @param location The location to adapt.
   * @return The adapted location.
   */
  def adaptLocation(location: ast.LocationAccess): Set[ast.LocationAccess] =
    location match {
      case ast.FieldAccess(receiver, field) =>
        val adapted = adaptReference(receiver)
        adapted.map { expression => ast.FieldAccess(expression, field)() }
      case location =>
        sys.error(s"Unexpected location: $location")
    }

  /**
   * Adapts the given expression.
   *
   * @param expression The expression to adapt.
   * @return The adapted expressions.
   */
  private def adapt(expression: ast.Exp): Set[ast.Exp] =
    if (expression.typ == ast.Ref) {
      adaptReference(expression)
    } else expression match {
      case equality: ast.EqCmp => adaptBinary(equality, ast.EqCmp(_, _)())
      case inequality: ast.NeCmp => adaptBinary(inequality, ast.NeCmp(_, _)())
      case _ => sys.error(s"Unexpected expression: $expression")
    }

  /**
   * Adapts the given binary expression.
   *
   * @param binary      The binary expression to adapt.
   * @param constructor The constructor used to build the adapted expressions.
   * @return THe adapted binary expressions.
   */
  private def adaptBinary(binary: ast.BinExp, constructor: (ast.Exp, ast.Exp) => ast.Exp): Set[ast.Exp] =
    for {
      left <- adapt(binary.left);
      right <- adapt(binary.right)
    } yield constructor(left, right)

  /**
   * Adapts the given reference-typed expression.
   *
   * @param expression The expression to adapt.
   * @return The adapted expressions.
   */
  private def adaptReference(expression: ast.Exp): Set[ast.Exp] = {
    val node = source.evaluateReference(expression)
    target.reachability.getOrElse(node, Set.empty)
  }
}
