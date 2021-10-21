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
import viper.silver.verifier.{Success, VerificationResult, Verifier}

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
  def createVerifier(configuration: Configuration): Verifier with Timing = {
    // create instance
    val instance = new Silicon with Timing {
      override def verify(program: ast.Program): VerificationResult =
        recordTime(super.verify(program))
    }
    // pass arguments
    val arguments = Seq(
      "--z3Exe", configuration.z3Exe,
      "--counterexample", "native",
      "--enableMoreCompleteExhale",
      "--ignoreFile", "dummy.vpr")
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
  def createSolver(configuration: Configuration): Solver with Timing = {
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

  override protected def createTeacher(input: Input, verifier: Verifier): AbstractTeacher =
    new Teacher(input, verifier)

  override protected def createLearner(input: Input, solver: Solver): AbstractLearner =
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
    // create input, verifier and solver
    implicit val input: Input = recordTime(Input.fromConfiguration(configuration))
    implicit val verifier: Verifier with Timing = recordTime {
      val verifier = createVerifier(configuration)
      verifier.start()
      verifier
    }
    implicit val solver: Solver with Timing = recordTime(createSolver(configuration))
    // infer hypothesis
    val (hypothesis, statistics) = recordTime(infer())
    // process inferred hypothesis
    val result = {
      val updated = statistics.withTimes(times)
      process(hypothesis, updated)
    }
    // stop verifier
    verifier.stop()
    // return processed result
    result
  }

  /**
   * Processes the given hypothesis and statistics.
   *
   * @param hypothesis The hypothesis.
   * @param statistics The statistics.
   * @param input      The input.
   * @param verifier   The verifier.
   * @return The result.
   */
  protected def process(hypothesis: Option[Hypothesis], statistics: Statistics)
                       (implicit input: Input, verifier: Verifier): R

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
 * An inference runner that prints the inferred hypothesis.
 */
trait PrintRunner extends Runner[Unit] {
  override protected def process(hypothesis: Option[Hypothesis], statistics: Statistics)
                                (implicit input: Input, verifier: Verifier): Unit =
    hypothesis match {
      case Some(hypothesis) =>
        // extend input program
        val extended = extend(input, hypothesis)
        // print statistics
        println(s"iterations: ${statistics.iterations}")
        println(s"samples: ${statistics.samples}")
        println(s"verifier time: ${statistics.verifierTime}")
        println(s"solver time: ${statistics.solverTime}")
        println(s"total time: ${statistics.time}")
        // print extended program
        println(extended)
      case None =>
        println("Unable to infer specifications.")
    }
}

/**
 * An inference runner that verifiers the program annotated with the inferred specification.
 */
trait TestRunner extends Runner[Boolean] {
  override protected def process(hypothesis: Option[Hypothesis], statistics: Statistics)
                                (implicit input: Input, verifier: Verifier): Boolean =
    hypothesis match {
      case Some(hypothesis) =>
        // extend input program
        val extended = extend(input, hypothesis)
        // verify extended program
        doesVerify(extended)
      case None =>
        false
    }

  /**
   * Returns whether the given program verifies.
   *
   * @param program  The program to verify.
   * @param verifier The verifier.
   * @return True if the program verifies.
   */
  private def doesVerify(program: ast.Program)(implicit verifier: Verifier): Boolean = {
    // verify program
    val result = verifier.verify(program)
    // process result
    result match {
      case Success => true
      case _ => false
    }
  }
}
