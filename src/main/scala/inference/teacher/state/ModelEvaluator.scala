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
import viper.silver.ast

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
            sys.error(s"Comparison of unsupported sort: $sort")
        }
      case other =>
        sys.error(s"Unexpected boolean term: $other")
    }

  /**
   * Evaluates the given term term to a permission value (represented as an integer).
   *
   * @param term The term to evaluate.
   * @return The permission value.
   */
  def evaluatePermission(term: Term): Int =
    term match {
      case terms.NoPerm() => 0
      case terms.FullPerm() => 1
      case terms.Var(identifier, _) =>
        val value = getString(identifier.name)
        val double = value.toDouble
        if (double.isWhole) double.toInt
        else sys.error("Unexpected fractional permission.")
      case terms.PermPlus(left, right) =>
        val leftValue = evaluatePermission(left)
        val rightValue = evaluatePermission(right)
        leftValue + rightValue
      case terms.Ite(condition, left, right) =>
        val boolean = evaluateBoolean(condition)
        if (boolean) evaluatePermission(left)
        else evaluatePermission(right)
      case other =>
        sys.error(s"Unexpected permission term: $other")
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
      case terms.App(applicable, arguments) if arguments.isEmpty =>
        val identifier = applicable.id
        getString(identifier.name)
      case terms.App(applicable, arguments) =>
        val identifier = applicable.id.name.replaceAll("\\[", "<").replaceAll("\\]", ">")
        val entry = model.entries.get(identifier).get.asInstanceOf[MapEntry]
        val argVals = arguments.map(evaluateReference)
        val argSeq = argVals.map(av => ConstantEntry(av))
        if (entry.options.contains(argSeq)) {
          return entry.options.get(argSeq).get.toString
        }
        entry.default.toString
      case other =>
        sys.error(s"Unexpected reference term: $other")
    }


  def evaluateDomainFunc(name: String, t: ast.Type, args: Seq[String]): Option[String] = {
    val internalNamePrefix = s"${name}<"
    val entry = model.entries.find(_._1.startsWith(internalNamePrefix))
    entry match {
      case Some(aEntry) =>
        val mEntry = aEntry._2.asInstanceOf[MapEntry]
        for (op <- mEntry.options) {
          if (op._1.length == args.length){
            val zipped = op._1.zip(args)
            val zippedNames = zipped.map(p => p._1.toString)
            if (zippedNames == args) {
              return Some(op._2.toString)
            }
          }
        }
        return Some(mEntry.default.toString)
      case None =>
    }
    return None
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
