/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import com.typesafe.scalalogging.Logger
import inference.core.sample.Sample
import inference.input.{Configuration, Input}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * An abstract inference.
 */
trait Inference {
  /**
   * Returns the logger.
   *
   * @return The logger.
   */
  protected def logger: Logger =
    Logger("root")

  /**
   * Infers a hypothesis using the given teacher and learner.
   *
   * @param teacher The teacher.
   * @param learner The learner.
   * @return The inferred hypothesis and the number of iterations it took.
   */
  def infer(teacher: AbstractTeacher, learner: AbstractLearner): (Option[Hypothesis], Int) = {
    // get maximum iteration number
    val max = teacher
      .input
      .configuration
      .iterations

    /**
     * Helper method used to iteratively compute the hypothesis.
     *
     * @param hypothesis The current hypothesis.
     * @param iteration  The current iteration number.
     * @return The final hypothesis and the number of iterations.
     */
    def iterate(hypothesis: Hypothesis, iteration: Int = 1): (Option[Hypothesis], Int) = {
      logger.info(s"iteration #$iteration")
      // check hypothesis
      val samples = teacher.check(hypothesis)
      // process samples
      if (samples.isEmpty) {
        // we are done if there are no new samples
        (Some(hypothesis), iteration)
      } else if (iteration > max) {
        // we abort if we have exceeded the iteration limit
        (None, iteration)
      } else {
        // add samples
        learner.addSamples(samples)
        // compute updated hypothesis and iterate
        learner
          .hypothesis
          .map { updated => iterate(updated, iteration + 1) }
          .getOrElse((None, iteration))
      }
    }

    // compute initial hypothesis and iterate
    val initial = learner.initial
    iterate(initial)
  }
}

/**
 * An abstract teacher of an inference.
 */
trait AbstractTeacher {
  /**
   * Returns the logger.
   *
   * @return The logger.
   */
  protected def logger: Logger =
    Logger("root.teacher")

  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  def input: Input

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
   * Returns the logger.
   *
   * @return The logger.
   */
  protected def logger: Logger =
    Logger("root.learner")

  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  def input: Input

  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  protected def configuration: Configuration =
    input.configuration

  /**
   * The current escalation level.
   */
  protected var level: Int = 0

  /**
   * The buffer used to accumulate samples
   */
  private val buffer: mutable.Buffer[Sample] =
    ListBuffer.empty

  /**
   * Returns whether there is a higher template complexity level.
   *
   * @return True if it is possible to escalate.
   */
  protected def canEscalate: Boolean =
    configuration.escalation && level < configuration.maxClauses

  /**
   * Escalates the template complexity level.
   */
  protected def escalate(): Unit =
    level = level + 1

  /**
   * Returns the number of clauses that may be used per guard.
   *
   * @return The number of clauses.
   */
  protected def clauseCount: Int =
    if (configuration.escalation) level
    else configuration.maxClauses

  /**
   * Adds the given sample.
   *
   * @param sample The sample to add.
   */
  def addSample(sample: Sample): Unit =
    buffer.append(sample)

  /**
   * Adds the given samples.
   *
   * @param samples The samples to add.
   */
  def addSamples(samples: Seq[Sample]): Unit =
    samples.foreach(addSample)

  /**
   * Returns all samples.
   *
   * @return The samples.
   */
  def samples: Seq[Sample] =
    buffer.toSeq

  /**
   * Returns the initial hypothesis.
   *
   * @return The hypothesis.
   */
  def initial: Hypothesis

  /**
   * Returns a hypothesis that is consistent with all samples.
   *
   * @return The hypothesis.
   */
  def hypothesis: Option[Hypothesis]
}
