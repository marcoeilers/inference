/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.VerificationRunner
import inference.input.Configuration
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import viper.silver.verifier.Verifier

import java.io.File
import scala.util.Properties

/**
 * Inference test.
 */
class InferenceTest extends AnyFunSuite with BeforeAndAfterAll with VerificationRunner with TestInputs {
  /**
   * The paths to the tests.
   */
  val directories: Seq[String] =
    Seq("/tests")

  override protected def roots: Seq[File] =
    directories.map { directory =>
      val path = getClass
        .getResource(directory)
        .getPath
      new File(path)
    }

  override protected val verifier: Verifier = {
    val arguments = Seq(
      "--z3Exe", Properties.envOrNone("Z3_EXE").get,
      "--ignoreFile", "dummy.vpr")
    createVerifier(arguments)
  }

  // run all tests
  runAllTests()

  override protected def beforeAll(): Unit = {
    verifier.start()
  }

  override protected def afterAll(): Unit = {
    verifier.stop()
  }

  /**
   * Runs all tests.
   */
  private def runAllTests(): Unit = {
    inputs.foreach { configuration =>
      val name = s"${configuration.input} [${configuration.arguments.mkString(" ")}]"
      runTest(name, configuration)
    }
  }

  /**
   * Runs a test with the given name and configurations.
   *
   * @param name          The name.
   * @param configuration The configuration.
   */
  private def runTest(name: String, configuration: Configuration): Unit =
    test(name) {
      val result = run(configuration)
      assert(result)
    }
}
