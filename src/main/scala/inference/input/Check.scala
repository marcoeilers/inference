/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.input

import inference.analysis.DepthAnalysis
import inference.core.{Hypothesis, Placeholder}
import viper.silver.ast
import viper.silver.ast.pretty.FastPrettyPrinter._
import viper.silver.ast.pretty.PrettyPrintPrimitives

/**
 * A check.
 */
sealed trait Check {
  /**
   * The depth function.
   */
  private lazy val depthFunction =
    DepthAnalysis.analyze(body)

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

  /**
   * Returns the body of the check.
   *
   * @return The body.
   */
  def body: ast.Seqn

  /**
   * Returns the depth up to which predicates should be folded and unfolded.
   *
   * @param hypothesis The current hypothesis.
   * @return The depth.
   */
  def depth(hypothesis: Hypothesis): Int =
    depthFunction(hypothesis)
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

sealed trait InferenceStatement extends ast.ExtensionStmt {
  // default meta data
  override val pos: ast.Position = ast.NoPosition
  override val info: ast.Info = ast.NoInfo
  override val errT: ast.ErrorTrafo = ast.NoTrafos

  // no sub-nodes
  override def extensionSubnodes: Seq[ast.Node] =
    Seq.empty
}

/**
 * An auxiliary statement used to mark instrumented statements.
 *
 * @param body        The body containing the instrumented statements.
 * @param annotations Some annotations (only present if annotations are enabled).
 */
case class Instrumented(body: ast.Seqn, annotations: Seq[Annotation]) extends InferenceStatement {
  override def prettyPrint: PrettyPrintPrimitives#Cont =
    text("instrumented") <+> showBlock(body)
}

/**
 * An auxiliary statement used to cut out loops to make the inference modular.
 *
 * @param loop The corresponding loop check.
 */
case class Cut(loop: LoopCheck) extends InferenceStatement {
  override def prettyPrint: PrettyPrintPrimitives#Cont =
    text(loop.name)
}
