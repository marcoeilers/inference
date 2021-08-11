/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.analysis

import inference.core.Hypothesis
import viper.silver.ast

/**
 * Analysis used to determine the depth up to which predicates should be folded and unfolded.
 */
object DepthAnalysis {
  /**
   * The type of depth functions.
   */
  type Depth = Hypothesis => Int

  /**
   * Analyzes the given statement.
   *
   * @param statement The statement to analyze.
   * @return The depth function.
   */
  def analyze(statement: ast.Stmt): Depth = {
    // TODO: Implement properly.
    Function.const(1)
  }
}
