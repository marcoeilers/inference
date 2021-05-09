/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.runner

import inference.core.{Hypothesis, Inference}
import inference.learner.Learner
import inference.teacher.Teacher

/**
 * An inference runner.
 *
 * @tparam R The result type.
 */
trait Runner[R] extends Inference {
  override protected def createTeacher(input: Input): Teacher =
    new Teacher()

  override protected def createLearner(input: Input): Learner =
    new Learner

  /**
   * Runs the inference with the given arguments.
   *
   * @param arguments The arguments.
   * @return The result.
   */
  def run(arguments: Seq[String]): R = {
    val configuration = new Configuration(arguments)
    run(configuration)
  }

  /**
   * Runs the inference with the given configuration.
   *
   * @param configuration The configuration.
   * @return The result.
   */
  def run(configuration: Configuration): R = {
    val input = Input(configuration)
    run(input)
  }

  /**
   * Runs the inference with the given input.
   *
   * @param input The input.
   * @return The result.
   */
  def run(input: Input): R = {
    val hypothesis = infer(input)
    result(input, hypothesis)
  }

  /**
   * Computes the result from the given input and inferred hypothesis.
   *
   * @param input      The input.
   * @param hypothesis The inferred hypothesis.
   * @return The result.
   */
  def result(input: Input, hypothesis: Hypothesis): R
}

/**
 * An inference runner that prints the inferred hypothesis.
 */
trait PrintRunner extends Runner[Unit] {
  override def result(input: Input, hypothesis: Hypothesis): Unit =
    println(hypothesis)
}
