/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.builder

import inference.Names
import inference.core.Hypothesis
import inference.input.{Check, Cut, Input}
import viper.silver.ast

/**
 * A program extender.
 */
trait ProgramExtender extends CheckExtender[ast.Seqn] {
  /**
   * Extends the program given by the input with specifications corresponding to the given hypothesis.
   *
   * @param input      The input to the inference.
   * @param hypothesis The inferred hypothesis.
   * @return The extended program.
   */
  def extend(implicit input: Input, hypothesis: Hypothesis): ast.Program = {
    // get configuration and original input program
    val configuration = input.configuration
    val original = input.program
    // fields
    val fields = {
      val extra =
        if (configuration.useHeuristics()) Seq(magic)
        else Seq.empty
      original.fields ++ extra
    }
    // predicates
    val predicates = {
      // extend existing predicates
      val extended = original
        .predicates
        .map { predicate =>
          val body = predicate.body
          if (body.isDefined) predicate
          else {
            val name = predicate.name
            val placeholder = input.placeholder(name)
            hypothesis.getPredicate(placeholder)
          }
        }
      // get recursive predicate
      val recursive =
        if (configuration.useRecursion()) {
          val placeholder = input.placeholder(Names.recursive)
          val predicate = hypothesis.getPredicate(placeholder)
          Seq(predicate)
        } else Seq.empty
      // combine predicates
      extended ++ recursive
    }
    // methods
    val methods = original
      .methods
      .map(extendMethod)
    // update program
    original.copy(
      fields = fields,
      predicates = predicates,
      methods = methods
    )(original.pos, original.info, original.errT)
  }

  /**
   * Extends the given method.
   *
   * @param method     The method to extend.
   * @param hypothesis The implicitly passed hypothesis.
   * @return The extended method.
   */
  private def extendMethod(method: ast.Method)(implicit input: Input, hypothesis: Hypothesis): ast.Method = {
    // get method specification
    val name = method.name
    val check = input.methodCheck(name)
    val precondition = hypothesis.getBody(check.precondition.asInstance)
    val postcondition = hypothesis.getBody(check.postcondition.asInstance)
    // extend method body
    val body = extendCheck(check)
    // update method
    method.copy(
      pres = Seq(precondition),
      posts = Seq(postcondition),
      body = Some(body)
    )(method.pos, method.info, method.errT)
  }

  override protected def processCheck(check: Check)(implicit hypothesis: Hypothesis): ast.Seqn =
    extendSequence(check.body)

  override protected def extendNonControlStatement(statement: ast.Stmt)(implicit hypothesis: Hypothesis): Unit =
    statement match {
      case ast.Inhale(predicate: ast.PredicateAccessPredicate) =>
        emitInhale(predicate)
        emitUnfold(predicate)
      case ast.Exhale(predicate: ast.PredicateAccessPredicate) =>
        emitFold(predicate)
        emitExhale(predicate)
      case Cut(loop) =>
        // get loop specification
        val invariant = hypothesis.getBody(loop.invariant.asInstance)
        // extend loop body
        val body = extendCheck(loop)
        // update loop
        val extended = {
          val original = loop.original
          loop
            .original.copy(
            invs = Seq(invariant),
            body = body
          )(original.pos, original.info, original.errT)
        }
        emit(extended)
      case other =>
        emit(other)
    }
}
