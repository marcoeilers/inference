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
import inference.util.Timing
import inference.util.solver.Solver
import viper.silver.verifier.Verifier

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
   * Creates a teacher with the given input and verifier.
   *
   * @param input    The input to the inference.
   * @param verifier The verifier.
   * @return The teacher.
   */
  protected def createTeacher(input: Input, verifier: Verifier): AbstractTeacher

  /**
   * Creates a learner with the given input and solver.
   *
   * @param input  The input to the inference.
   * @param solver The solver.
   * @return The learner.
   */
  protected def createLearner(input: Input, solver: Solver): AbstractLearner

  /**
   * Infers a hypothesis.
   *
   * @param input    The input.
   * @param verifier The verifier used by the teacher to check hypotheses.
   * @param solver   The solver used by the learner to generate hypotheses.
   * @return The inferred hypothesis and some statistics.
   */
  def infer()(implicit input: Input, verifier: Verifier with Timing, solver: Solver with Timing): (Option[Hypothesis], Statistics) = {
    // create teacher and learner
    val teacher = createTeacher(input, verifier)
    val learner = createLearner(input, solver)
    // get maximum iteration number
    val max = input.configuration.iterations

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
    val (hypothesis, iterations) = iterate(initial)

    // compute statistics
    val statistics = Statistics(
      iterations = iterations,
      samples = learner.samples.size,
      verifierTimes = verifier.times,
      solverTimes = solver.times
    )
    // return final hypothesis and statistics
    (hypothesis, statistics)
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
  protected def input: Input

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

/**
 * An object carrying statistical information.
 *
 * @param iterations    The number of iterations.
 * @param samples       The number of samples.
 * @param verifierTimes The times recorded by the verifier.
 * @param solverTimes   The times recorded by the solver.
 * @param times         The times recorded by the inference (this includes verifier and solver times).
 */
case class Statistics(iterations: Int, samples: Int, verifierTimes: Seq[Long], solverTimes: Seq[Long], times: Seq[Long] = Seq.empty) {
  /**
   * Returns the total verifier time.
   *
   * @return The verifier time.
   */
  def verifierTime: Long =
    verifierTimes.sum

  /**
   * Returns the total solver time.
   *
   * @return The solver time.
   */
  def solverTime: Long =
    solverTimes.sum

  /**
   * Returns the total time.
   *
   * @return The total time
   */
  def time: Long =
    times.sum

  /**
   * Recurs a copy of the statistics with the given recorded times.
   *
   * @param times The recorded times.
   * @return The updated statistics.
   */
  def withTimes(times: Seq[Long]): Statistics =
    copy(times = times)
}
