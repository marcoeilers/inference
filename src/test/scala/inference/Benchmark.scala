/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.DefaultRunner
import inference.input.Configuration

import java.io.File
import scala.xml.{Node, XML}

/**
 * The benchmark.
 */
object Benchmark extends DefaultRunner {
  /**
   * The path to the root directory of the benchmark.
   */
  private val benchmark = "/benchmark"

  /**
   * The main method of the benchmark.
   *
   * @param arguments The input
   */
  def main(arguments: Array[String]): Unit = {
    // get root directory of benchmark
    val root = {
      val resource = getClass.getResource(benchmark)
      new File(resource.getPath)
    }
    // get configurations and run inference for each of them
    val configurations = collectConfigurations(root)
    configurations.foreach { configuration =>
      // TODO: Process statistics.
      val (_, statistics) = run(configuration)
      println(statistics)
    }
  }

  /**
   * Collects configurations from the given directory by recursively searching through all subdirectories.
   *
   * @param directory The directory.
   * @return The configurations.
   */
  private def collectConfigurations(directory: File): Seq[Configuration] = {
    // collected configurations from nested directories
    val children = directory.listFiles
    val nested = children
      .filter(_.isDirectory)
      .flatMap(collectConfigurations)
    // collect configurations from current directory
    val collected = {
      // get all input files (*.vpr)
      val files = children.filter(_.isFile)
      val inputs = files
        .map(_.getPath)
        .filter(_.endsWith(".vpr"))
      // get all configurations and combine them with inputs
      files
        .filter(_.getPath.endsWith(".xml"))
        .flatMap(readConfigurations)
        .flatMap { configuration =>
          inputs.map { input =>
            configuration.withInput(input)
          }
        }
    }
    // combine results
    nested ++ collected
  }

  /**
   * Reads a sequence of configurations from the given file.
   *
   * @param file The file.
   * @return The configurations.
   */
  private def readConfigurations(file: File): Seq[Configuration] = {
    val xml = XML.loadFile(file)
    extractConfigurations(xml)
  }

  /**
   * Extracts a sequence of configurations form the given xml node.
   *
   * @param xml The xml node.
   * @return The configurations.
   */
  private def extractConfigurations(xml: Node): Seq[Configuration] =
    (xml \ "options").map { options =>
      // process options
      val arguments = (options \ "option").flatMap { option =>
        val key = (option \ "@key").map { attribute => s"--${attribute.text}" }
        val value = (option \ "@value").map(_.text)
        key ++ value
      }
      // create configuration
      println(arguments.mkString(" "))
      Configuration(arguments)
    }
}
