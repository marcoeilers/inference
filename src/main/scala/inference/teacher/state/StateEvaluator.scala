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
  private[state] val map = label
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
   * Returns the value associated with the given variable.
   *
   * @param variable The variable to look up.
   * @return The value.
   */
  private def store(variable: ast.LocalVar): String = {
    // adapt variable to state (if necessary)
    val adapted = label match {
      case Some(label) =>
        // adapt variable
        val name = s"${label}_${variable.name}"
        val typ = variable.typ
        ast.LocalVar(name, typ)()
      case _ =>
        // no adaptation needed
        variable
    }
    // evaluate variable
    val term = state.g(adapted)
    model.evaluateReference(term)
  }

  /**
   * Optionally returns the value associated with the given heap node and field.
   *
   * @param node  The node.
   * @param field The field.
   * @return The value.
   */
  def heapOption(node: String, field: String): Option[String] =
    map
      .get(node)
      .flatMap(_.get(field))

  /**
   * Optionally evaluates the given expression (assumed to be boolean-typed).
   *
   * @param expression The expression.
   * @return The boolean value.
   */
  def evaluateBooleanOption(expression: ast.Exp): Option[Boolean] =
    expression match {
      case binary@ast.BinExp(left, right) =>
        left.typ match {
          case ast.Bool =>
            for {
              leftValue <- evaluateBooleanOption(left)
              rightValue <- evaluateBooleanOption(right)
            } yield binary match {
              case _: ast.And => leftValue && rightValue
              case other => sys.error(s"Unexpected binary expression: $other")
            }
          case ast.Ref =>
            for {
              leftValue <- evaluateReferenceOption(left)
              rightValue <- evaluateReferenceOption(right)
            } yield binary match {
              case _: ast.EqCmp => leftValue == rightValue
              case _: ast.NeCmp => leftValue != rightValue
              case other => sys.error(s"Unexpected binary expression: $other")
            }
        }
      case other =>
        sys.error(s"Unexpected expression: $other")
    }

  /**
   * Evaluates the given expression (assumed to be boolean-typed).
   * TODO: Use optional implementation above if it does not introduce performance issues.
   *
   * @param expression The expression to evaluate.
   * @return The boolean value.
   */
  def evaluateBoolean(expression: ast.Exp): Boolean =
    expression match {
      case binary@ast.BinExp(left, right) =>
        left.typ match {
          case ast.Bool =>
            // evaluate operands
            val leftValue = evaluateBoolean(left)
            val rightValue = evaluateBoolean(right)
            // reduce operands
            binary match {
              case _: ast.And => leftValue && rightValue
              case _ => sys.error(s"Unexpected binary expression: $binary")
            }
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
   * Optionally evaluates the given expression (assumed to be reference-typed).
   *
   * @param expression The expression to evaluate.
   * @return The reference value.
   */
  def evaluateReferenceOption(expression: ast.Exp): Option[String] =
    expression match {
      case ast.NullLit() =>
        val value = model.evaluateReference(terms.Null())
        Some(value)
      case variable: ast.LocalVar =>
        val value = store(variable)
        Some(value)
      case ast.FieldAccess(receiver, field) =>
        val value = evaluateReferenceOption(receiver)
        value.flatMap { node => heapOption(node, field.name) }
      case other =>
        sys.error(s"Unexpected expression: $other")
    }

  /**
   * Evaluates the given expression (assumed to be reference-typed).
   * TODO: Use optional implementation above if it does not introduce performance issues.
   *
   * @param expression The expression to evaluate.
   * @return The reference value.
   */
  def evaluateReference(expression: ast.Exp): String =
    expression match {
      case ast.NullLit() =>
        model.evaluateReference(terms.Null())
      case variable: ast.LocalVar =>
        store(variable)
      case ast.FieldAccess(receiver, ast.Field(field, _)) =>
        val receiverValue = evaluateReference(receiver)
        map(receiverValue)(field)
    }
}
