/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.input.Configuration

import java.io.File
import scala.xml.{Node, PrettyPrinter, XML}

/**
 * A trait providing methods to collect test inputs.
 */
trait TestInputs {
  /**
   * Returns a sequence of paths to the directories containing the test inputs.
   *
   * @return The paths.
   */
  protected def roots: Seq[String]

  /**
   * Returns a sequence of configurations representing the test inputs.
   *
   * @return The configurations representing the inputs.
   */
  protected def inputs: Seq[Configuration] =
    roots
      .map { directory =>
        val path = getClass
          .getResource(directory)
          .getPath
        new File(path)
      }
      .flatMap(collectConfigurations)

  /**
   * Collects the inputs from the given directory by recursively searching through all subdirectories and pairing the
   * input files with the configurations meant for that input.
   *
   * @param directory The directory.
   * @return The configurations.
   */
  private def collectConfigurations(directory: File): Seq[Configuration] = {
    // get subdirectories and files
    val children = directory
      .listFiles
      .toSeq
      .sortBy(_.getPath)
    // collect configurations from nested directories
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
      // get all configurations
      val y = files
        .filter(_.getPath.endsWith(".xml"))
      val configurations = y
        .flatMap(loadConfigurations)
      // combine inputs and configurations
      inputs.flatMap { input =>
        configurations.map(_.withInput(input))
      }
    }
    // combine results
    nested ++ collected
  }

  /**
   * Loads a sequence of configurations from the given file.
   *
   * @param file The file.
   * @return The configurations.
   */
  private def loadConfigurations(file: File): Seq[Configuration] = {
    val xml = XML.loadFile(file)
    extractConfigurations(xml)
  }

  val printer = new PrettyPrinter(80, 4)

  /**
   * Extracts a sequence of configurations from the given XML node.
   *
   * @param xml The XML node.
   * @return The configurations.
   */
  private def extractConfigurations(xml: Node): Seq[Configuration] = {
    (xml \ "options").map { options =>
      val extracted = extractOptions(options)
      Configuration(extracted)
    }
  }

  /**
   * Extracts options from the given XML node.
   *
   * @param xml The XML node.
   * @return The options.
   */
  private def extractOptions(xml: Node): Seq[String] = {
    (xml \ "option").flatMap { option =>
      // get key
      val key = option
        .attribute("key")
        .map { attribute => s"--${attribute.text}" }
      assert(key.isDefined)
      // get optional value
      val value = option
        .attribute("value")
        .map(_.text)
      // add key and value to sequence
      key ++ value
    }
  }
}
