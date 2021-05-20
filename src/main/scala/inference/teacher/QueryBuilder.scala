/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.Names
import inference.core.{Hypothesis, Instance}
import inference.runner.Input
import inference.util.{Builder, Namespace}
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A query builder mixin.
 */
trait QueryBuilder extends Builder {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * The namespace used to generate unique identifiers.
   */
  private var namespace: Namespace = _

  /**
   * The partial query.
   */
  private var query: PartialQuery = _

  /**
   * Builds a query that checks the given hypothesis.
   *
   * @param hypothesis The hypothesis to check.
   * @return The query.
   */
  protected def buildQuery(hypothesis: Hypothesis): Query = {
    // reset
    reset()
    // build program
    val program = instrumentProgram(input.program, hypothesis)
    // finalize query
    query(program)
  }

  /**
   * Resets the query builder.
   */
  private def reset(): Unit = {
    namespace = input.namespace.copy()
    query = new PartialQuery
  }

  /**
   * Instruments the given program.
   *
   * @param program    The program to instrument.
   * @param hypothesis The current hypothesis.
   * @return The instrumented program.
   */
  private def instrumentProgram(program: ast.Program, hypothesis: Hypothesis): ast.Program = {
    val methods = program
      .methods
      .map { method => instrumentMethod(method)(hypothesis) }
    program.copy(methods = methods)(program.pos, program.info, program.errT)
  }

  /**
   * Instruments the given method.
   *
   * @param method     The method to instrument.
   * @param hypothesis The implicitly passed current hypothesis.
   * @return The instrumented method.
   */
  private def instrumentMethod(method: ast.Method)(implicit hypothesis: Hypothesis): ast.Method = {
    method.body match {
      case Some(body) =>
        val instrumented = makeScope {
          method.pres.foreach { expression => instrumentStatement(ast.Inhale(expression)()) }
          instrumentStatement(body)
          method.posts.foreach { expression => instrumentStatement(ast.Exhale(expression)()) }
        }
        // update method
        method.copy(
          pres = Seq.empty,
          posts = Seq.empty,
          body = Some(instrumented)
        )(method.pos, method.info, method.errT)
      case _ =>
        sys.error("Missing method body.")
    }
  }

  /**
   * Instruments the given sequence.
   *
   * @param sequence   The sequence to instrument.
   * @param hypothesis The implicitly passed current hypothesis.
   * @return The instrumented sequence.
   */
  private def instrumentSequence(sequence: ast.Seqn)(implicit hypothesis: Hypothesis): ast.Seqn = {
    val statements = scoped(sequence.ss.foreach(instrumentStatement))
    sequence.copy(ss = statements)(sequence.pos, sequence.info, sequence.errT)
  }

  /**
   * Instruments the given statement.
   *
   * @param statement  The statement to instrument.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def instrumentStatement(statement: ast.Stmt)(implicit hypothesis: Hypothesis): Unit =
    statement match {
      case sequence: ast.Seqn =>
        val instrumented = instrumentSequence(sequence)
        emit(instrumented)
      case conditional: ast.If =>
        // instrument branches
        val thenBranch = instrumentSequence(conditional.thn)
        val elseBranch = instrumentSequence(conditional.els)
        // update conditional
        val instrumented = conditional.copy(
          thn = thenBranch,
          els = elseBranch
        )(conditional.pos, conditional.info, conditional.errT)
        emit(instrumented)
      case ast.Inhale(ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _)) =>
        // get instance
        val instance = input
          .placeholders(name)
          .asInstance(arguments)
        // inhale specification
        // TODO: Inhale existing specification
        val body = hypothesis.get(name)
        emitInhale(body)
        // save snapshot
        saveSnapshot(instance)
      case ast.Exhale(ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _)) =>
        // get instance
        val instance = input
          .placeholders(name)
          .asInstance(arguments)
        // save snapshot
        saveSnapshot(instance)
        // exhale specification
        // TODO: Exhale existing specification
        val body = hypothesis.get(name)
        emitExhale(body)
      case other =>
        emit(other)
    }

  /**
   * Saves a snapshot of the given instance.
   *
   * @param instance The instance.
   */
  private def saveSnapshot(instance: Instance): Unit = {
    // generate unique snapshot label
    val name = namespace.uniqueIdentifier(Names.snapshot)
    query.addSnapshot(name, instance)
    // emit label
    emitLabel(name)
  }
}

/**
 * A partial query.
 */
private class PartialQuery {
  /**
   * The buffer used to accumulate the snapshots.
   */
  private val snapshots: mutable.Buffer[(String, Instance)] =
    ListBuffer.empty

  /**
   * Adds a snapshot, i.e., associates the given name with the given placeholder instance.
   *
   * @param name     The name of the snapshot.
   * @param instance The instance saved by the snapshot.
   */
  def addSnapshot(name: String, instance: Instance): Unit =
    snapshots.append(name -> instance)

  /**
   * Finalizes the query with the given program.
   *
   * @param program The program.
   * @return The finalized query.
   */
  def apply(program: ast.Program): Query =
    Query(program, snapshots.toSeq)
}
