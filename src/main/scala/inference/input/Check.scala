/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.core.Placeholder
import viper.silver.ast
import viper.silver.ast.Node
import viper.silver.ast.pretty.FastPrettyPrinter._
import viper.silver.ast.pretty.PrettyPrintPrimitives

/**
 * A check.
 */
sealed trait Check {
  /**
   * Returns the name of the check.
   *
   * @return The name.
   */
  def name: String

  /**
   * The original node, i.e., method or loop, the check corresponds to.
   *
   * @return The original node.
   */
  def original: ast.Node
}

/**
 * A check corresponding to a method.
 *
 * @param original      The original method.
 * @param precondition  The precondition placeholder.
 * @param postcondition The postcondition placeholder.
 * @param body          The check body.
 */
case class MethodCheck(original: ast.Method, precondition: Placeholder, postcondition: Placeholder, body: ast.Seqn) extends Check {
  override def name: String =
    original.name
}

/**
 * A check corresponding to a loop.
 *
 * @param original  The original loop.
 * @param name      The name of the check.
 * @param invariant The invariant placeholder.
 * @param body      The check body.
 */
case class LoopCheck(original: ast.While, name: String, invariant: Placeholder, body: ast.Seqn) extends Check {
  /**
   * Returns the loop condition.
   *
   * @return The loop condition.
   */
  def condition: ast.Exp =
    original.cond
}

/**
 * An auxiliary statement used to cut out loops to make the inference modular.
 *
 * @param loop The corresponding loop check.
 */
case class Cut(loop: LoopCheck) extends ast.ExtensionStmt {
  // default meta data
  override val pos: ast.Position = ast.NoPosition
  override val info: ast.Info = ast.NoInfo
  override val errT: ast.ErrorTrafo = ast.NoTrafos

  override def extensionSubnodes: Seq[Node] =
    Seq.empty

  override def prettyPrint: PrettyPrintPrimitives#Cont =
    text(loop.name)
}