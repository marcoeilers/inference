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
import inference.util.solver.{Solver, Z3Solver}
import viper.silicon.Silicon
import viper.silver.ast
import viper.silver.verifier.{Success, Verifier}

/**
 * An inference runner.
 *
 * @tparam R The result type.
 */
trait Runner[R] extends Inference {
  /**
   * Creates a verifier with the given configuration.
   *
   * @param configuration The configuration.
   * @return The verifier.
   */
  def createVerifier(configuration: Configuration): Verifier = {
    // create instance
    val instance = new Silicon()
    // pass arguments
    val arguments = Seq(
      "--z3Exe", configuration.z3Exe(),
      "--counterexample", "raw",
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
  def createSolver(configuration: Configuration): Solver = {
    // create and initialize solver
    val solver = new Z3Solver(configuration.z3Exe())
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
  def run(arguments: Seq[String]): Option[R] = {
    val configuration = new Configuration(arguments)
    run(configuration)
  }

  /**
   * Runs the inference with the given configuration.
   *
   * @param configuration The configuration.
   * @return The result.
   */
  def run(configuration: Configuration): Option[R] = {
    // create input
    val input = Input.fromConfiguration(configuration)
    // create verifier and solver
    implicit val verifier: Verifier = createVerifier(configuration)
    implicit val solver: Solver = createSolver(configuration)
    // before
    verifier.start()
    // run
    val result = run(input)
    // after
    verifier.stop()
    // return result
    result
  }

  /**
   * Runs the inference with the given input.
   *
   * @param input    The input.
   * @param verifier The verifier.
   * @param solver   The solver.
   * @return The result.
   */
  def run(input: Input)(implicit verifier: Verifier, solver: Solver): Option[R] = {
    val hypothesis = infer(input)
    result(input, hypothesis)
  }

  /**
   * Computes the result from the given input and inferred hypothesis.
   *
   * @param input      The input.
   * @param hypothesis The inferred hypothesis.
   * @param verifier   The verifier.
   * @param solver     The solver.
   * @return The result.
   */
  def result(input: Input, hypothesis: Option[Hypothesis])(implicit verifier: Verifier, solver: Solver): Option[R]
}

/**
 * An inference runner that prints the inferred hypothesis.
 */
trait PrintRunner extends Runner[Unit] with ProgramExtender {
  override def result(input: Input, hypothesis: Option[Hypothesis])(implicit verifier: Verifier, solver: Solver): Option[Unit] = {
    hypothesis match {
      case Some(hypothesis) =>
        // extend input program
        val extended = extend(input, hypothesis)
        // print extended program
        println(extended)
        Some()
      case None =>
        println("Unable to infer specifications.")
        None
    }
  }
}

/**
 * An inference runner that verifiers the program annotated with the inferred specification.
 */
trait TestRunner extends Runner[Boolean] with ProgramExtender {
  override def result(input: Input, hypothesis: Option[Hypothesis])(implicit verifier: Verifier, solver: Solver): Option[Boolean] =
    hypothesis.map { hypothesis =>
      // extend input program
      val extended = extend(input, hypothesis)
      // verify extended program
      doesVerify(extended)
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
