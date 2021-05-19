/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.runner.TestRunner
import org.scalatest.funsuite.AnyFunSuite
import viper.silver.utility.Paths

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * Inference test.
 */
class InferenceTest extends AnyFunSuite with TestRunner {
  /**
   * The path to the tests.
   */
  val directory = "/tests"

  // run all tests
  runAll()

  /**
   * Runs all tests.
   */
  def runAll(): Unit = {
    // get files to test
    val files = {
      val resource = getClass.getResource(directory)
      val path = Paths.pathFromResource(resource)
      Files.newDirectoryStream(path).asScala.toSeq.map(_.toString)
    }

    // run tests
    files.foreach { path =>
      val arguments = Seq(path)
      runTest(path, arguments)
    }
  }

  /**
   * Runs a test with the given name and arguments.
   *
   * @param name      The name of the test.
   * @param arguments The arguments to the inference.
   */
  def runTest(name: String, arguments: Seq[String]): Unit =
    test(name) {
      val verified = run(arguments)
      assert(verified)
    }
}
