/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.StatisticsRunner
import inference.input.Configuration

import java.io.File
import scala.xml.{Node, XML}

/**
 * The benchmark.
 */
object Benchmark extends StatisticsRunner {
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
    // collect inputs and run them with the associated configurations
    val inputs = collectInputs(root)
    val outputs = inputs.map { case (input, configurations) =>
      val results = configurations.map { configuration =>
        run(configuration)
      }
      (input, results)
    }
  }

  /**
   * Collects the inputs from the given directory by recursively searching through all subdirectories and pairing the
   * input files with a list of configurations meant for that input.
   *
   * @param directory The directory.
   * @return The inputs.
   */
  private def collectInputs(directory: File): Seq[(String, Seq[Configuration])] = {
    // get subdirectories and files
    val children = directory
      .listFiles
      .toSeq
    // collected configurations from nested directories
    val nested = children
      .filter(_.isDirectory)
      .flatMap(collectInputs)
    // collect configurations from current directory
    val collected = {
      // get all input files (*.vpr)
      val files = children.filter(_.isFile)
      val inputs = files
        .map(_.getPath)
        .filter(_.endsWith(".vpr"))
      // get all configurations
      val configurations = files
        .filter(_.getPath.endsWith(".xml"))
        .flatMap(readConfigurations)
      // combine configurations with inputs
      inputs.map { input =>
        val updated = configurations.map(_.withInput(input))
        input -> updated
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
