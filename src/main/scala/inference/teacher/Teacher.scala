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
import viper.silver.verifier.Verifier

/**
 * The default implementation of the teacher.
 *
 * @param input    The input to the inference.
 * @param verifier The verifier used to check the hypotheses.
 */
class Teacher(input: Input, verifier: Verifier) extends AbstractTeacher {
  override def check(hypothesis: Hypothesis): Seq[Sample] =
    Seq.empty
}
