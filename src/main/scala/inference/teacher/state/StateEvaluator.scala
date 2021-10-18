/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import inference.teacher.state.StateEvaluator.{Heap, Store}
import inference.util.ast.Infos
import viper.silicon.interfaces.SiliconNativeCounterexample
import viper.silicon.interfaces.state.Chunk
import viper.silver.ast
import viper.silicon.resources.FieldID
import viper.silicon.state.terms
import viper.silicon.state.terms.sorts
import viper.silicon.state.BasicChunk

/**
 * State evaluator companion object.
 */
object StateEvaluator {
  /**
   * Type shorthand for counter examples.
   */
  private type Counter = SiliconNativeCounterexample

  /**
   * The type shortcut for stores.
   */
  type Store = Map[String, String]

  /**
   * The type shortcut for heaps.
   */
  type Heap = Map[String, Map[String, String]]

  /**
   * Extracts a state evaluator for the state with the given label from the given counter-example.
   *
   * @param label   The label of the state.
   * @param counter The counter-example.
   * @return The state evaluator.
   */
  def apply(label: String, counter: Counter): StateEvaluator = {
    val model = ModelEvaluator(counter.model)
    val store = processStore(counter.store, model)
    val heap = {
      val native = counter.oldHeaps(label)
      processHeap(native, model)
    }
    StateEvaluator(Some(label), store, heap, model)
  }

  /**
   * Helper method that processes the given Silicon store.
   *
   * @param store The store to process.
   * @param model The model.
   * @return The processed store.
   */
  def processStore(store: Map[String, terms.Term], model: ModelEvaluator): Store = {
    store.flatMap { case (name, term) =>
      term.sort match {
        case sorts.Ref =>
          val value = model.evaluateReference(term)
          Some(name -> value)
        case _ =>
          // ignore non-reference variables
          None
      }
    }
  }

  /**
   * Helper method that processes the given Silicon heap.
   *
   * @param heap  The heap to process.
   * @param model The model.
   * @return The processed heap.
   */
  def processHeap(heap: Iterable[Chunk], model: ModelEvaluator): Heap =
    heap.foldLeft(Map.empty: Heap) {
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
            // ignore non-reference fields
            result
        }
      case (result, _) =>
        result
    }
}

/**
 * A state evaluator.
 *
 * @param label The label of the state snapshot.
 * @param store The store.
 * @param heap  The heap.
 * @param model The model evaluator.
 */
case class StateEvaluator(label: Option[String], store: Store, heap: Heap, model: ModelEvaluator) {
  /**
   * Returns the value associated with the given variable.
   *
   * @param variable The variable to look up.
   * @return The value.
   */
  private def lookup(variable: ast.LocalVar): String = {
    // get variable name
    val name = label match {
      case Some(label) if !Infos.isSaved(variable) =>
        // adapt name
        s"${label}_${variable.name}"
      case _ =>
        variable.name
    }
    // lookup value
    store(name)
  }

  /**
   * Optionally returns the value associated with the given heap node and field.
   *
   * @param node  The node.
   * @param field The field.
   * @return The value.
   */
  private def lookup(node: String, field: String): Option[String] =
    heap
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
        val value = lookup(variable)
        Some(value)
      case ast.FieldAccess(receiver, field) =>
        val value = evaluateReferenceOption(receiver)
        value.flatMap { node => lookup(node, field.name) }
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
        lookup(variable)
      case ast.FieldAccess(receiver, ast.Field(field, _)) =>
        val receiverValue = evaluateReference(receiver)
        heap(receiverValue)(field)
    }
}
