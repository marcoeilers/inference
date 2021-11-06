/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher.state

import inference.Names
import inference.core.sample.{FieldAbstraction, PredicateAbstraction, ResourceAbstraction}
import inference.core.{Hypothesis, Instance}
import inference.input.Input
import viper.silver.ast

/**
 * A helper class used to evaluate permission amounts represented by specifications.
 *
 * @param input      The input to the inference.
 * @param hypothesis The current hypothesis.
 * @param state      The state used for the evaluation.
 */
class PermissionEvaluator(input: Input, hypothesis: Hypothesis, state: StateEvaluator) {
  /**
   * Evaluates the permission amount for the given resource represented by the specification corresponding to the given
   * specification instance.
   *
   * @param resource The resource.
   * @param instance The specification instance.
   * @param depth    The depth up to which predicates should be unfolded.
   * @return The permission amount.
   */
  def evaluate(resource: ResourceAbstraction, instance: Instance, depth: Int): Int = {
    val specification = hypothesis.getInferred(instance)
    evaluate(resource, specification, depth)
  }

  /**
   * Evaluates the permission amount for the given resource represented by the given specification.
   *
   * @param resource      The resource.
   * @param specification The specification.
   * @param depth         The depth up to which predicates should be unfolded.
   * @return The permission amount.
   */
  def evaluate(resource: ResourceAbstraction, specification: ast.Exp, depth: Int): Int =
    specification match {
      case ast.TrueLit() => 0
      case ast.And(left, right) =>
        val leftValue = evaluate(resource, left, depth)
        val rightValue = evaluate(resource, right, depth)
        leftValue + rightValue
      case ast.Implies(guard, guarded) =>
        val condition = state
          .evaluateBooleanOption(guard)
          .getOrElse(false)
        if (condition) evaluate(resource, guarded, depth) else 0
      case ast.FieldAccessPredicate(access, _) =>
        resource match {
          case FieldAbstraction(receiver, field) =>
            if (field == access.field) {
              val condition = receiver.abstracts(access.rcv)
              if (condition) 1 else 0
            } else 0
          case _ =>
            // TODO: Implement me properly
            0
        }
      case ast.PredicateAccessPredicate(access, _) =>
        resource match {
          case resource: FieldAbstraction =>
            if (depth > 0) {
              val instance = input.instance(access)
              val nested = hypothesis.getInferred(instance)
              evaluate(resource, nested, depth - 1)
            } else 0
          case PredicateAbstraction(name, arguments) =>
            // lazily compute whether all arguments are equal
            lazy val equalArguments = arguments
              .zip(access.args)
              .forall { case (abstraction, argument) => abstraction.abstracts(argument) }
            // check whether resource and specification are the same
            if (name == access.predicateName && equalArguments) 1
            else {
              // TODO: Implement me properly
              if (depth > 0 && Names.isRecursive(access.predicateName)) {
                val instance = input.instance(access)
                val nested = hypothesis.getInferred(instance)
                evaluate(resource, nested, depth - 1)
              } else 0
            }
        }
      case _: ast.DomainFuncApp =>
        0
      case other =>
        sys.error(s"Unexpected specification: $other")
    }

  /**
   * Evaluates the given boolean expression.
   *
   * @param expression The expression to evaluate.
   * @return The value.
   */
  def evaluateBoolean(expression: ast.Exp): Boolean =
    state
      .evaluateBooleanOption(expression)
      .getOrElse(false)
}
