/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.core.Instance
import viper.silver.ast

/**
 * A query.
 *
 * @param program   The program representing the query.
 * @param snapshots A sequence containing labels and placeholder instances for all state snapshots.
 */
case class Query(program: ast.Program, snapshots: Seq[(String, Instance)])
