/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.sample.Sample
import inference.core.{AbstractTeacher, Hypothesis}
import inference.input.Input
import viper.silver.verifier.{Failure, Success, VerificationError, Verifier}

/**
 * The default implementation of the teacher.
 *
 * @param input    The input to the inference.
 * @param verifier The verifier used to check the hypotheses.
 */
class Teacher(protected val input: Input, verifier: Verifier) extends AbstractTeacher with QueryBuilder with SampleExtractor {
  override def check(hypothesis: Hypothesis): Seq[Sample] =
    basicChecks(hypothesis).getOrElse(framingCheck(hypothesis))

  /**
   * Performs basic checks under consideration of the given hypothesis and returns the extracted samples. If the
   * hypothesis contains specifications that are not self-framed, nothing is returned.
   *
   * @param hypothesis The current hypothesis.
   * @return The extracted samples or nothing.
   */
  private def basicChecks(hypothesis: Hypothesis): Option[Seq[Sample]] = {
    val batches = input.batches
    val empty = Option(Seq.empty[Sample])
    batches.foldRight(empty) {
      case (batch, Some(samples)) =>
        val query = basicQuery(batch, hypothesis)
        execute(query, error => extractBasicSample(query, error)).map(samples ++ _)
      case (_, None) => None
    }
  }

  /**
   * Performs a well-formedness check.
   *
   * @param hypothesis The current hypothesis.
   * @return The extracted samples.
   */
  private def framingCheck(hypothesis: Hypothesis): Seq[Sample] = {
    val query = framingQuery(hypothesis)
    execute(query, error => extractFramingSample(query, error)).get
  }

  /**
   * Executes the given query and uses the given sample extraction function to generate samples in the case of an
   * unsuccessful verification. If the query yields a well-formedness error, nothing is returned.
   *
   * @param query   The query.
   * @param extract The sample extraction function.
   * @return The extracted samples or nothing.
   */
  private def execute(query: Query, extract: VerificationError => Sample): Option[Seq[Sample]] = {
    // verify program
    val program = query.program
    val result = verifier.verify(program)
    // extract sample from failure
    result match {
      case Success =>
        Some(Seq.empty)
      case Failure(errors) =>
        // filter non-verification errors
        val filtered = errors.collect {
          case error: VerificationError => error
          case error => sys.error(s"Unexpected verification failure: $error")
        }
        // check whether there are no well-formedness errors
        val wellformed = filtered.forall(_.id != "predicate.not.wellformed")
        if (wellformed) {
          // extract samples
          val samples = filtered.map(extract)
          Some(samples)
        } else None
    }
  }
}
