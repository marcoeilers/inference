/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.TestRunner
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
      val path = getPath(directory)
      collectFiles(path)
    }

    // run tests
    files.foreach { path =>
      val file = path.toString
      val arguments = Seq(file)
      runTest(file, arguments)
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

  /**
   * Takes a path represented as a string and converts it into a path object.
   *
   * @param name The path as a string.
   * @return The path object.
   */
  private def getPath(name: String): Path = {
    val resource = getClass.getResource(name)
    Paths.pathFromResource(resource)
  }

  /**
   * Collects all files contained in the directory with the given path.
   *
   * @param path The path to the directory.
   * @return The files.
   */
  private def collectFiles(path: Path): Seq[Path] =
    if (Files.isDirectory(path)) {
      Files
        .newDirectoryStream(path)
        .asScala
        .toSeq
        .flatMap(collectFiles)
    } else {
      Seq(path)
    }
}
