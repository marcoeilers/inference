/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import com.typesafe.scalalogging.Logger
import inference.core.{AbstractLearner, Hypothesis}
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
  override def hypothesis: Option[Hypothesis] = {
    if (samples.isEmpty) {
      // return empty hypothesis
      val initial = Hypothesis(Seq.empty)
      Some(initial)
    } else {
      // generate templates
      val templates = generateTemplates()
      // encode guards, solve constraints and build hypothesis
      val model = solve(templates)
      model.map { model => buildHypothesis(templates, model) }
    }
  }
}
