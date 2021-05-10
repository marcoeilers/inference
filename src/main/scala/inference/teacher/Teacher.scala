/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.{AbstractTeacher, Hypothesis, Sample}
import inference.runner.Input
import viper.silver.verifier.{Failure, Success, VerificationError, Verifier}

/**
 * The default implementation of the teacher.
 *
 * @param input    The input to the inference.
 * @param verifier The verifier used to check the hypotheses.
 */
class Teacher(protected val input: Input, verifier: Verifier) extends AbstractTeacher with QueryBuilder with SampleExtractor {
  override def check(hypothesis: Hypothesis): Seq[Sample] = {
    val query = buildQuery(hypothesis)
    execute(query, error => extractSample(query, error))
  }

  /**
   * Executes the given query and uses the given sample extraction function to generate samples in the case of an
   * unsuccessful verification.
   *
   * @param query   The query.
   * @param extract The sample extraction function.
   * @return The extracted samples.
   */
  private def execute(query: Query, extract: VerificationError => Sample): Seq[Sample] = {
    // verify program
    val program = query.program
    val result = verifier.verify(program)
    // extract sample from failure
    result match {
      case Success =>
        Seq.empty
      case Failure(errors) =>
        errors.map {
          case error: VerificationError => extract(error)
          case error => sys.error(s"Unexpected verification failure: $error")
        }
    }
  }
}
