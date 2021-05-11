/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import viper.silicon.state.terms
import viper.silicon.state.terms.{Term, sorts}
import viper.silver.verifier.{ApplicationEntry, ConstantEntry, MapEntry, Model, ModelEntry, ValueEntry}

/**
 * A model evaluator.
 *
 * @param model The model.
 */
case class ModelEvaluator(model: Model) {
  /**
   * The map from snapshots to reference values used to evaluate wrapped snapshots.
   */
  private lazy val snapshots =
    getEntry(key = "$SortWrappers.$SnapTo$Ref") match {
      case MapEntry(options, default) => options
        .map { case (Seq(snapshot), value) => snapshot -> value.toString }
        .withDefaultValue(default.toString)
    }

  /**
   * Evaluates the given term to a boolean value.
   *
   * @param term The term to evaluate.
   * @return The boolean value.
   */
  def evaluateBoolean(term: Term): Boolean =
    term match {
      case terms.True() => true
      case terms.False() => false
      case terms.Var(identifier, _) =>
        val value = getString(identifier.name)
        value.toBoolean
      case terms.Not(argument) =>
        !evaluateBoolean(argument)
      case terms.Equals(left, right) =>
        left.sort match {
          case sorts.Ref =>
            val leftValue = evaluateReference(left)
            val rightValue = evaluateReference(right)
            leftValue == rightValue
          case sort =>
            sys.error(s"Comparison of unsupported sort: ${sort}")
        }
    }

  /**
   * Evaluates the given term to a reference value (represented as a string).
   *
   * @param term The term to evaluate.
   * @return The reference value.
   */
  def evaluateReference(term: Term): String =
    term match {
      case terms.Null() =>
        getString(key = "$Ref.null")
      case terms.Var(identifier, _) =>
        getString(identifier.name)
      case terms.SortWrapper(wrapped, _) =>
        wrapped.sort match {
          case sorts.Snap =>
            val value = evaluateSnapshot(wrapped)
            snapshots(value)
          case sort =>
            sys.error(s"Unexpected sort: $sort")
        }
    }

  /**
   * Evaluates the given term to a snapshot value (represented as a value entry).
   *
   * @param term The term to evaluate.
   * @return The snapshot value.
   */
  def evaluateSnapshot(term: Term): ValueEntry =
    term match {
      case terms.Var(identifier, _) =>
        getValueEntry(identifier.name)
      case terms.First(combined) =>
        val value = evaluateSnapshot(combined)
        value match {
          case ApplicationEntry(_, Seq(first, _)) => first
          case entry => sys.error(s"Unexpected value entry: $entry")
        }
      case terms.Second(combined) =>
        val value = evaluateSnapshot(combined)
        value match {
          case ApplicationEntry(_, Seq(_, second)) => second
          case entry => sys.error(s"Unexpected value entry: $entry")
        }
    }


  /**
   * Returns the model entry associated with the given key as a string value.
   *
   * @param key The key.
   * @return The string value.
   */
  private def getString(key: String): String =
    getEntry(key) match {
      case ConstantEntry(value) => value
      case entry => sys.error(s"Unexpected model entry: $entry")
    }

  /**
   * Returns the value entry associated with the given key.
   *
   * @param key The key.
   * @return The value entry.
   */
  private def getValueEntry(key: String): ValueEntry =
    getEntry(key) match {
      case value: ValueEntry => value
      case entry => sys.error(s"Unexpected model entry: $entry")
    }

  /**
   * Returns the model entry associated with the given key.
   *
   * @param key The key.
   * @return The model entry.
   */
  private def getEntry(key: String): ModelEntry =
    model.entries(key)
}
