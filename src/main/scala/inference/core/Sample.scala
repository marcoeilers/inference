/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.core

import viper.silver.ast

/**
 * A sample.
 */
trait Sample

/**
 * A lower bound sample.
 *
 * @param record The record.
 */
case class LowerBound(record: Record) extends Sample

/**
 * A record representing a data point.
 *
 * @param placeholder The specification placeholder corresponding to this record.
 * @param resources   The set of expressions that can be used to represent the resource in question.
 */
case class Record(placeholder: Placeholder, resources: Set[ast.LocationAccess])
