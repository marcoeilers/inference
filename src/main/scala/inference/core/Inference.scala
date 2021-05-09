/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.runner.Input

import scala.annotation.tailrec

/**
 * An abstract inference.
 */
trait Inference {
  /**
   * Creates a teacher with the given input.
   *
   * @param input The input.
   * @return The teacher.
   */
  protected def createTeacher(input: Input): AbstractTeacher

  /**
   * Creates a learner with the given input.
   *
   * @param input The input.
   * @return The learner.
   */
  protected def createLearner(input: Input): AbstractLearner

  /**
   * Infers a hypothesis for the given input.
   *
   * @param input The input.
   * @return The inferred hypothesis.
   */
  def infer(input: Input): Hypothesis = {
    // create teacher and learner
    val teacher = createTeacher(input)
    val learner = createLearner(input)

    /**
     * Helper method used to iteratively compute the hypothesis.
     *
     * @param hypothesis The current hypothesis.
     * @return The final hypothesis.
     */
    @tailrec
    def iterate(hypothesis: Hypothesis): Hypothesis = {
      // check hypothesis
      val samples = teacher.check(hypothesis)
      // check if there are new samples
      if (samples.isEmpty) hypothesis
      else {
        // add samples to learner
        learner.addSamples(samples)
        // compute updated hypothesis and iterate
        val updated = learner.hypothesis
        iterate(updated)
      }
    }

    // compute initial hypothesis and iterate
    val initial = learner.hypothesis
    iterate(initial)
  }
}

/**
 * An abstract teacher of an inference.
 */
trait AbstractTeacher {
  /**
   * Checks the given hypothesis.
   *
   * @param hypothesis The hypothesis to check.
   * @return The samples.
   */
  def check(hypothesis: Hypothesis): Seq[Sample]
}

/**
 * An abstract learner of an inference.
 */
trait AbstractLearner {
  /**
   * Adds the given sample.
   *
   * @param sample The sample to add.
   */
  def addSample(sample: Sample): Unit

  /**
   * Adds the given samples.
   *
   * @param samples The samples to add.
   */
  def addSamples(samples: Seq[Sample]): Unit =
    samples.foreach(addSample)

  /**
   * Returns a hypothesis that is consistent with all samples.
   *
   * @return The hypothesis.
   */
  def hypothesis: Hypothesis
}
