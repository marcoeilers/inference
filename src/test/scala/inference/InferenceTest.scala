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
  val directory: String = "/tests"

  /**
   * The path to the tests meant to be executed using the default settings.
   */
  val defaultDirectory: String =
    s"$directory/default"

  /**
   * The path to the tests meant to be executed using predicate segments.
   */
  val segmentsDirectory: String =
    s"$directory/segments"

  // run all tests
  runAll()

  /**
   * Runs all tests.
   */
  private def runAll(): Unit = {
    // run tests using default settings
    val defaultFiles = collectFiles(defaultDirectory)
    defaultFiles.foreach(runDefaultTest)

    // run tests using predicate segments
    val segmentsFiles = defaultFiles ++ collectFiles(segmentsDirectory)
    segmentsFiles.foreach(runSegmentsTest)
  }

  /**
   * Tests the given file using the default settings.
   *
   * @param file The file to test.
   */
  private def runDefaultTest(file: String): Unit = {
    val name = s"test using default settings: $file"
    val arguments = Main.defaultOptions ++ Seq(file)
    runTest(name, arguments)
  }

  /**
   * Tests the given file using predicate segments.
   *
   * @param file The file to test.
   */
  private def runSegmentsTest(file: String): Unit = {
    val name = s"test using predicate segments: $file"
    val arguments = Main.segmentsOptions ++ Seq(file)
    runTest(name, arguments)
  }

  /**
   * Runs a test with the given name and arguments.
   *
   * @param name      The name of the test.
   * @param arguments The arguments to the inference.
   */
  private def runTest(name: String, arguments: Seq[String]): Unit =
    test(name) {
      val result = run(arguments)
      assert(result)
    }

  /**
   * Collects all files contained in the directory with the given name.
   *
   * @param name The name of the directory.
   * @return The files.
   */
  private def collectFiles(name: String): Seq[String] = {
    val resource = getClass.getResource(name)
    if (resource != null) {
      val path = Paths.pathFromResource(resource)
      val files = collectFiles(path)
      files.map(_.toString)
    } else {
      logger.warn(s"Directory does not exist: $name")
      Seq.empty
    }
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
