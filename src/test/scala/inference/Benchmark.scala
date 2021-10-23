/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference

import inference.core.StatisticsRunner

import scala.xml.PrettyPrinter

/**
 * The benchmark.
 */
object Benchmark extends StatisticsRunner with TestInputs {
  override protected def roots: Seq[String] =
    Seq("/benchmark")

  /**
   * The main method of the benchmark.
   *
   * @param arguments The arguments to the benchmark.
   */
  def main(arguments: Array[String]): Unit = {
    // run inference on all inputs
    val statistics = inputs.map { configuration =>
      run(configuration)
    }
    // TODO: Do something with the statistics.
    val pretty = new PrettyPrinter(80, 3)
    statistics.foreach { statistics =>
      val xml = statistics.toXml
      println(pretty.format(xml))
    }
  }
}
