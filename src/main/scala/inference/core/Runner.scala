/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import inference.builder.ProgramExtender
import inference.input.{Configuration, Input}
import inference.learner.Learner
import inference.teacher.Teacher
import inference.util.Timing
import inference.util.solver.{Solver, Z3Solver}
import viper.silicon.Silicon
import viper.silver.ast
import viper.silver.verifier.{Failure, Success, VerificationResult, Verifier}

/**
 * An inference runner.
 *
 * @tparam R The result type.
 */
trait Runner[R] extends Inference with Timing {
  /**
   * Creates a verifier with the given configuration.
   *
   * @param configuration The configuration.
   * @return The verifier.
   */
  protected def createVerifier(configuration: Configuration): Verifier with Timing = {
    // get arguments
    val arguments = Seq(
      "--z3Exe", configuration.z3Exe,
      "--counterexample", "native",
      "--enableMoreCompleteExhale",
      "--ignoreFile", "dummy.vpr")
    // create verifier
    createVerifier(arguments)
  }

  /**
   * Creates a verifier with the given arguments.
   *
   * @param arguments The arguments.
   * @return The verifier.
   */
  protected def createVerifier(arguments: Seq[String]): Verifier with Timing = {
    // create instance
    val instance = new Silicon with Timing {
      override def verify(program: ast.Program): VerificationResult =
        recordTime(super.verify(program))
    }
    // pass arguments
    instance.parseCommandLine(arguments)
    // return instance
    instance
  }

  /**
   * Creates a solver with the given configuration.
   *
   * @param configuration The configuration.
   * @return The solver.
   */
  protected def createSolver(configuration: Configuration): Solver with Timing = {
    // create solver
    val solver = new Z3Solver(configuration.z3Exe) with Timing {
      override def solve(): Option[Map[String, Boolean]] =
        recordTime(super.solve())
    }
    // initialize solver
    solver.initialize()
    // return solver
    solver
  }

  /**
   * Creates a teacher with the given input and verifier.
   *
   * @param input    The input to the inference.
   * @param verifier The verifier.
   * @return The teacher.
   */
  protected def createTeacher(input: Input, verifier: Verifier): AbstractTeacher =
    new Teacher(input, verifier)

  /**
   * Creates a learner with the given input and solver.
   *
   * @param input  The input to the inference.
   * @param solver The solver.
   * @return The learner.
   */
  protected def createLearner(input: Input, solver: Solver): AbstractLearner =
    new Learner(input, solver)

  /**
   * Runs the inference with the given arguments.
   *
   * @param arguments The arguments.
   * @return The result.
   */
  def run(arguments: Seq[String]): R = {
    val configuration = Configuration(arguments)
    run(configuration)
  }

  /**
   * Runs the inference with the given configuration.
   *
   * @param configuration The configuration.
   * @return The result.
   */
  def run(configuration: Configuration): R = {
    // process input
    val (input, inputTime) = time {
      Input.fromConfiguration(configuration)
    }
    // create verifier and solver
    val ((verifier, solver), startupTime) = time {
      val verifier = createVerifier(configuration)
      val solver = createSolver(configuration)
      verifier.start()
      (verifier, solver)
    }
    // run inference
    val ((hypothesis, iterations, samples), inferenceTime) = time {
      val teacher = createTeacher(input, verifier)
      val learner = createLearner(input, solver)
      val (hypothesis, iterations) = infer(teacher, learner)
      val samples = learner.samples.size
      (hypothesis, iterations, samples)
    }
    // stop verifier
    verifier.stop()
    // create statistics
    val statistics = Statistics(
      input = configuration.input,
      options = configuration.arguments,
      success = hypothesis.isDefined,
      iterations = iterations,
      samples = samples,
      inputTime = inputTime,
      startupTime = startupTime,
      inferenceTime = inferenceTime,
      verifierTimes = verifier.times,
      solverTimes = solver.times
    )
    // process inferred hypothesis and statistics
    process(input, hypothesis, statistics)
  }

  /**
   * Processes the given inferred hypothesis and statistics.
   *
   * @param input      The input to the inference.
   * @param hypothesis The hypothesis.
   * @param statistics The statistics.
   * @return The result.
   */
  protected def process(input: Input, hypothesis: Option[Hypothesis], statistics: Statistics): R

  /**
   * Extends the program corresponding to the given input with the specifications represented by the given hypothesis.
   *
   * @param input      The input.
   * @param hypothesis The hypothesis.
   * @return The extended program.
   */
  protected def extend(input: Input, hypothesis: Hypothesis): ast.Program = {
    val extender = new ProgramExtender(input)
    extender.extend(hypothesis)
  }
}

/**
 * An inference runner that returns the statistics
 */
trait StatisticsRunner extends Runner[Statistics] {
  override protected def process(input: Input, hypothesis: Option[Hypothesis], statistics: Statistics): Statistics =
    statistics
}

/**
 * An inference runner that extends the input program with the inferred specifications.
 */
trait ExtensionRunner extends Runner[Option[ast.Program]] {
  override protected def process(input: Input, hypothesis: Option[Hypothesis], statistics: Statistics): Option[ast.Program] =
    hypothesis.map { hypothesis => extend(input, hypothesis) }
}

/**
 * An inference runner that extends the input program with the inferred specifications and then checks wheter the
 * extended program verifies.
 */
trait VerificationRunner extends Runner[Boolean] {
  /**
   * Returns the verifier.
   *
   * @return The verifier.
   */
  protected def verifier: Verifier

  /**
   * Returns whether the given program verifies.
   *
   * @param program The program to verify.
   * @return True if the program verifies.
   */
  protected def doesVerify(program: ast.Program): Boolean = {
    // verify program
    val result = verifier.verify(program)
    // check result
    result match {
      case Success => true
      case Failure(_) => false
    }
  }

  /**
   * Processes the given inferred hypothesis and statistics.
   *
   * @param input      The input to the inference.
   * @param hypothesis The hypothesis.
   * @param statistics The statistics.
   * @return The result.
   */
  override protected def process(input: Input, hypothesis: Option[Hypothesis], statistics: Statistics): Boolean =
    hypothesis.exists { hypothesis =>
      // extend input program
      val extended = extend(input, hypothesis)
      // verify extended program
      doesVerify(extended)
    }
}
