/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import inference.core.{Instance, Placeholder}
import inference.marco.Extension
import inference.util.collections.SetMap
import viper.silver.ast
import viper.silver.ast.NoTrafos

/**
 * A state snapshot.
 *
 * @param instance The placeholder instance corresponding to the state snapshot.
 * @param state    The state evaluator.
 */
case class Snapshot(instance: Instance, state: StateEvaluator) {
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
          case (map1, (node, expressions)) =>
            val currentCode = (state.heap
            .getOrElse(node, Map.empty)
            .foldLeft(map1) {
              case (map2, (name, (srt, value))) =>
                val extended = expressions.map { expression =>
                  val actualType = srt match {
                    case "Ref" => ast.Ref
                    case "Int" => ast.Int
                    case _ => ast.DomainType(srt, Map.empty)(Seq())
                  }
                  val field = ast.Field(name, actualType)()
                  ast.FieldAccess(expression, field)(): ast.Exp
                }
                SetMap.addAll(map2, value, extended)
            })
            val whatWasThat = state.heap.getOrElse(node, Map.empty)
            val domainFuncMap = state.domainFuncs
            val finalRes = if (domainFuncMap.contains(node)) {
              val domainFuncs = domainFuncMap(node)
              val domainFuncsMap = domainFuncs.flatMap { case (name, value) =>
                val typeName = name.substring(name.indexOf("<") + 1, name.indexOf(">"))
                val funcName = name.substring(0, name.indexOf("<"))
                val extended: Set[ast.Exp] = expressions.map { expression =>
                  val actualType = typeName match {
                    case "Ref" => ast.Ref
                    case "Int" => ast.Int
                    case _ => ast.DomainType(typeName, Map.empty)(Seq())
                  }
                  val domainFuncApp = ast.DomainFuncApp(funcName, Seq(expression), Extension.domainFuncAppTypeMap(funcName, actualType))(ast.NoPosition, ast.NoInfo, actualType, "", ast.NoTrafos)
                  domainFuncApp
                }
                SetMap.addAll(currentCode, value, extended)
              }
              SetMap.merge(currentCode, domainFuncsMap)
            } else {
              currentCode
            }
            finalRes
        }
        // recurse and combine result
        val future = recurse(next, steps - 1)
        SetMap.merge(current, future)
      }

    // initial reachability map
    val parameters = instance.placeholder.variables
    val initial = instance
      .arguments
      .zip(parameters)
      .foldLeft(Map.empty[String, Set[ast.Exp]]) {
        case (result, (argument, parameter)) =>
          if (true || argument.typ == ast.Ref) {
            val value = state.evaluateReferenceOption(argument)
            if (value.isDefined)
              SetMap.add(result, value.get, parameter)
            else
              result
          } else result
      }

    // recursively follow fields
    // TODO: Number of steps
    recurse(initial, steps = 3)
  }

  /**
   * Lazily computed reachability map that also includes null.
   */
  private[state] lazy val nullableReachability = {
    val literal = ast.NullLit()()
    val value = state.evaluateReference(literal)
    SetMap.add(reachability, value, literal)
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
    nullableReachability.map { case (_, set) => set }

  override def toString: String =
    instance.toString
}

/**
 * An adaptor used to adapt expressions from one state to another.
 *
 * @param source The source state.
 * @param target The target state.
 */
case class Adaptor(source: StateEvaluator, target: Snapshot) {
  /**
   * Optionally returns a set of expressions that may be used to represent the given reference-typed expression.
   *
   * @param expression The expression to adapt.
   * @param nullable   The flag indicating whether the set may contain the null reference.
   * @return The adapted expression.
   */
  def adapt(expression: ast.Exp, nullable: Boolean): Option[Set[ast.Exp]] = {
    // get reachability map
    val map =
      if (nullable) target.nullableReachability
      else target.reachability
    // adapt expression
    adapt(expression, map)
  }

  /**
   * Optionally adapts the given reference-typed expression using the given reachability map.
   *
   * @param expression The expression to adapt.
   * @param map        The reachability map.
   * @return The adapted expression.
   */
  private def adapt(expression: ast.Exp, map: Map[String, Set[ast.Exp]]): Option[Set[ast.Exp]] =
    source
      .evaluateReferenceOption(expression)
      .map { node => map.getOrElse(node, Set.empty) }
}
