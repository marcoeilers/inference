/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import viper.silver.ast
import viper.silicon.resources.FieldID
import viper.silicon.state.terms
import viper.silicon.state.terms.sorts
import viper.silicon.state.{BasicChunk, State}

/**
 * A state evaluator.
 *
 * @param label The label of the state snapshot.
 * @param state The silicon state.
 * @param model The model evaluator.
 */
case class StateEvaluator(label: Option[String], state: State, model: ModelEvaluator) {
  /**
   * The precomputed heap map.
   */
  private[state] val heap = label
    .map(state.oldHeaps)
    .getOrElse(state.h)
    .values
    .foldLeft(Map.empty[String, Map[String, String]]) {
      case (result, chunk: BasicChunk) if chunk.resourceID == FieldID =>
        val term = chunk.snap
        term.sort match {
          case sorts.Ref =>
            val receiver = model.evaluateReference(chunk.args.head)
            val field = chunk.id.name
            val value = model.evaluateReference(term)
            // update field map
            val fields = result
              .getOrElse(receiver, Map.empty)
              .updated(field, value)
            // update heap
            result.updated(receiver, fields)
          case _ =>
            // do nothing
            result
        }
      case (result, _) =>
        result
    }

  /**
   * Evaluates the given expression (assumed to be boolean-typed).
   *
   * @param expression The expression to evaluate.
   * @return The boolean value.
   */
  def evaluateBoolean(expression: ast.Exp): Boolean =
    expression match {
      case binary@ast.BinExp(left, right) =>
        left.typ match {
          case ast.Ref =>
            // evaluate operands
            val leftValue = evaluateReference(left)
            val rightValue = evaluateReference(right)
            // reduce operands
            binary match {
              case _: ast.EqCmp => leftValue == rightValue
              case _: ast.NeCmp => leftValue != rightValue
              case _ => sys.error(s"Unexpected binary expression: $binary")
            }
          case typ =>
            sys.error(s"Unexpected argument type: $typ")
        }
      case _ =>
        sys.error(s"Unexpected expression: $expression")
    }

  /**
   * Evaluates the given expression (assumed to be reference-typed).
   *
   * @param expression The expression to evaluate.
   * @return The reference value.
   */
  def evaluateReference(expression: ast.Exp): String =
    expression match {
      case ast.NullLit() =>
        model.evaluateReference(terms.Null())
      case variable: ast.LocalVar =>
        val term = state.g(variable)
        model.evaluateReference(term)
      case ast.FieldAccess(receiver, ast.Field(field, _)) =>
        val receiverValue = evaluateReference(receiver)
        heap(receiverValue)(field)
    }

  /**
   * Evaluates the permission variable with the given name.
   *
   * @param name The name of the variable.
   * @return The permission value.
   */
  def evaluatePermission(name: String): Double = {
    val variable = ast.LocalVar(name, ast.Perm)()
    val term = state.g(variable)
    model.evaluatePermission(term)
  }

  /**
   * Evaluates the permission amount for the given access represented by the given specification.
   *
   * @param access        The access.
   * @param specification The specification.
   * @return The permission amount.
   */
  def evaluatePermission(access: ast.LocationAccess, specification: ast.Exp): Int =
    specification match {
      case ast.TrueLit() => 0
      case ast.And(left, right) =>
        val leftValue = evaluatePermission(access, left)
        val rightValue = evaluatePermission(access, right)
        leftValue + rightValue
      case ast.Implies(guard, guarded) =>
        val condition = evaluateBoolean(guard)
        if (condition) evaluatePermission(access, guarded) else 0
      case ast.FieldAccessPredicate(x, _) =>
        access match {
          case ast.FieldAccess(receiver, field) =>
            if (field == x.field) {
              val comparison = ast.EqCmp(receiver, x.rcv)()
              val condition = evaluateBoolean(comparison)
              if (condition) 1 else 0
            } else 0
          case _ => 0
        }
      case _ =>
        ???
    }
}
