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
import inference.input.{Check, Configuration, Cut, Hint, Input}
import inference.util.ast.Statements
import viper.silver.ast

/**
 * A program extender.
 *
 * @param input The input.
 */
class ProgramExtender(val input: Input) extends CheckExtender[ast.Seqn] with Folding {
  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

  /**
   * Extends the input program with specifications corresponding to the given hypothesis.
   *
   * @param hypothesis The inferred hypothesis.
   * @return The extended program.
   */
  def extend(hypothesis: Hypothesis): ast.Program = {
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
      .map { method => extendMethod(method)(hypothesis) }
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
  private def extendMethod(method: ast.Method)(implicit hypothesis: Hypothesis): ast.Method = {
    // get method specification
    val name = method.name
    val check = input.methodCheck(name)
    val precondition = hypothesis.getBody(check.precondition.asInstance)
    val postcondition = hypothesis.getBody(check.postcondition.asInstance)
    // extend method body
    val body = {
      val extended = extendCheck(check)
      Statements.makeDeclared(extended, method.scopedDecls)
    }
    // update method
    method.copy(
      pres = Seq(precondition),
      posts = Seq(postcondition),
      body = Some(body)
    )(method.pos, method.info, method.errT)
  }

  override protected def processCheck(check: Check)(implicit hypothesis: Hypothesis): ast.Seqn =
    extendSequence(check.body)

  override protected def processInstrumented(statement: ast.Stmt)(implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit =
    statement match {
      case ast.Seqn(statements, _) =>
        statements.foreach(processInstrumented)
      case ast.Inhale(expression) =>
        expression match {
          case resource@ast.PredicateAccessPredicate(predicate, _) =>
            val instance = input.instance(predicate)
            if (instance.isPredicate) {
              // inhale and unfold predicate
              emitInhale(resource)
              emitUnfold(resource)
            } else {
              // unfold predicates appearing in specification
              implicit val maxDepth: Int =
                if (configuration.useAnnotations()) check.depth(hypothesis)
                else 0
              val body = hypothesis.getBody(instance)
              unfold(body)
            }
          case _ => // do nothing
        }
      case ast.Exhale(expression) =>
        expression match {
          case resource@ast.PredicateAccessPredicate(predicate, _) =>
            val instance = input.instance(predicate)
            if (instance.isPredicate) {
              // fold and exhale predicate
              emitFold(resource)
              emitExhale(resource)
            } else {
              // fold predicates appearing in specification
              val body = hypothesis.getBody(instance)
              if (configuration.useAnnotations()) {
                // fold with hints
                implicit val maxDepth: Int = check.depth(hypothesis)
                foldWithHints(body, hints)
              } else {
                // fold without hints
                implicit val maxDepth: Int = configuration.heuristicsFoldDepth()
                fold(body)
              }
            }
          case _ => // do nothing
        }
      case other =>
        emit(other)
    }

  override protected def processCut(cut: Cut)(implicit hypothesis: Hypothesis): Unit = {
    // get loop specification
    val check = cut.loop
    val invariant = hypothesis.getBody(check.invariant.asInstance)
    // extend loop body
    val body = extendCheck(check)
    // update loop
    val extended = {
      val original = check.original
      original.copy(
        invs = Seq(invariant),
        body = body
      )(original.pos, original.info, original.errT)
    }
    emit(extended)
  }
}
