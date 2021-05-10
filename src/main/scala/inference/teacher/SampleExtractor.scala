/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Sample
import viper.silver.verifier.VerificationError

/**
 * A sample extractor.
 */
trait SampleExtractor {
  /**
   * Extracts a sample from the given query and verification error.
   *
   * @param query The query that caused the error.
   * @param error The verification error.
   */
  def extractSample(query: Query, error: VerificationError): Sample = {
    // TODO: Implement me.
    ???
  }
}
