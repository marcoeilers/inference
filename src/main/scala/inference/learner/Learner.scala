/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.{AbstractLearner, _}

/**
 * The default implementation of the learner.
 */
class Learner extends AbstractLearner {
  override def addSample(sample: Sample): Unit =
    ()

  override def hypothesis: Hypothesis =
    Hypothesis()
}
