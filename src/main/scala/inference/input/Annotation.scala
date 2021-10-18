/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.Names
import viper.silver.ast

/**
 * A ghost code annotation.
 *
 * @param name      The name of the annotation.
 * @param arguments The arguments for which the annotation is meant.
 * @param saved     The variables holding the old values of the arguments.
 * @param flag      The variable used to flag whether the annotation is relevant.
 */
case class Annotation(name: String, arguments: Seq[ast.Exp], saved: Seq[ast.LocalVar], flag: ast.LocalVar) {
  /**
   * Returns whether this is an append annotation.
   *
   * @return True if this is an append annotation.
   */
  @inline
  def isAppend: Boolean =
    name == Names.appendAnnotation

  /**
   * Returns whether this is a concat annotation.
   *
   * @return True if this is a concat annotation.
   */
  @inline
  def isConcat: Boolean =
    name == Names.concatAnnotation
}
