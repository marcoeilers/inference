/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.{AbstractLearner, Hypothesis, Sample}
import inference.input.Input
import inference.util.solver.Solver

/**
 * The default implementation of the learner.
 *
 * @param input  The input to the inference.
 * @param solver The solver used to generate hypotheses.
 *
 */
class Learner(protected val input: Input, protected val solver: Solver)
  extends AbstractLearner
    with TemplateGenerator
    with HypothesisSolver
    with HypothesisBuilder {

  override def hypothesis: Hypothesis = {
    if (samples.isEmpty) {
      // return empty hypothesis
      Hypothesis(Seq.empty)
    } else {
      // generate templates
      val templates = generateTemplates()
      // encode guards and solve constraints
      val model = solve(templates)
      // build hypothesis
      buildHypothesis(templates, model)
    }
  }
}
