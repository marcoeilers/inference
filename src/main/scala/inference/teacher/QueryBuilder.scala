/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Hypothesis
import inference.runner.Input

/**
 * A query builder.
 */
trait QueryBuilder {
  /**
   * The input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Builds a query that checks the given hypothesis.
   *
   * @param hypothesis The hypothesis to check.
   * @return The query.
   */
  def buildQuery(hypothesis: Hypothesis): Query =
    Query(input.program)
}
