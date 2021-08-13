/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.Names
import inference.builder.{CheckExtender, Folding}
import inference.core.{Hypothesis, Instance}
import inference.input.{Check, Configuration, Cut, Input, LoopCheck, MethodCheck}
import inference.util.ast.{Statements, ValueInfo}
import inference.util.Namespace
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A query builder mixin.
 */
trait QueryBuilder extends CheckExtender[ast.Method] with Folding {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

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
    // get original program and checks
    val original = input.program
    val checks = input.checks
    // fields
    val fields = {
      val extra = if (configuration.useHeuristics()) Seq(magic) else Seq.empty
      original.fields ++ extra
    }
    // predicates
    val predicates = {
      // get placeholders
      val placeholders =
        if (configuration.noInlining()) input.placeholders
        else input.placeholders.filter(_.isRecursive)
      // get predicates
      placeholders.map(hypothesis.getPredicate)
    }
    // instrument methods
    implicit val current: Hypothesis = hypothesis
    val methods = checks.map(extendCheck)
    // instrument program
    val program = original.copy(
      fields = fields,
      predicates = predicates,
      methods = methods
    )(original.pos, original.info, original.errT)
    // finalize query
    query(program, hypothesis)
  }

  /**
   * Resets the query builder.
   */
  private def reset(): Unit = {
    namespace = new Namespace()
    query = new PartialQuery
  }

  override protected def processCheck(check: Check)(implicit hypothesis: Hypothesis): ast.Method =
    check match {
      case MethodCheck(original, precondition, postcondition, body) =>
        // instrument body
        val instrumented = makeDeclaredScope {
          inhaleInstance(precondition.asInstance)
          extendStatement(body)
          exhaleInstance(postcondition.asInstance)
        }
        // build method based on original
        original.copy(
          pres = Seq.empty,
          posts = Seq.empty,
          body = Some(instrumented)
        )(original.pos, original.info, original.errT)
      case check@LoopCheck(_, name, invariant, body) =>
        // instrument loop
        val instrumented = makeDeclaredScope {
          inhaleInstance(invariant.asInstance)
          emitInhale(check.condition)
          extendStatement(body)
          exhaleInstance(invariant.asInstance)
        }
        // build method
        ast.Method(name, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Some(instrumented))()
    }

  override protected def extendNonControlStatement(statement: ast.Stmt)(implicit hypothesis: Hypothesis): Unit =
    statement match {
      case ast.Inhale(ast.PredicateAccessPredicate(predicate, _)) =>
        // get and inhale instance
        val instance = input.instance(predicate)
        inhaleInstance(instance)
      case ast.Exhale(ast.PredicateAccessPredicate(predicate, _)) =>
        // get and exhale instance
        val instance = input.instance(predicate)
        exhaleInstance(instance)
      case call@ast.MethodCall(name, arguments, targets) =>
        val check = input.methodCheck(name)
        // exhale method precondition (method's precondition was replaced with true)
        val precondition = check
          .precondition
          .asInstance(arguments)
        exhaleInstance(precondition)
        // emit method call (to havoc targets)
        emit(call)
        // inhale method postcondition (method's postcondition was replaced with true)
        val postcondition = check
          .postcondition
          .asInstance(arguments ++ targets)
        inhaleInstance(postcondition)
      case Cut(loop) =>
        // exhale loop invariant
        val invariant = loop.invariant.asInstance
        exhaleInstance(invariant)
        // havoc written variables
        val havoc = Statements.makeHavoc(loop.original.writtenVars)
        emit(havoc)
        // inhale loop invariant and negated loop condition
        inhaleInstance(invariant)
        emitInhale(ast.Not(loop.condition)())
      case other =>
        emit(other)
    }

  /**
   * Inhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def inhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis): Unit = {
    // get body
    val body = hypothesis.getBody(instance)
    // inhale specification
    // TODO: Inhale existing specification
    if (configuration.noInlining()) {
      val resource = instance.asResource
      emitInhale(resource)
      emitUnfold(resource)
    } else {
      emitInhale(body)
    }
    // unfold and save
    if (configuration.useHeuristics()) {
      val maxDepth = 0
      unfold(body)(maxDepth, hypothesis)
    } else {
      // TODO: Use hints
      val maxDepth = check.depth(hypothesis)
      unfold(body)(maxDepth, hypothesis)
    }
    saveSnapshot(instance, exhaled = false)
  }

  /**
   * Exhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def exhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis): Unit = {
    // get body
    val body = hypothesis.getBody(instance)
    // save and fold
    saveSnapshot(instance, exhaled = true)
    if (configuration.useHeuristics()) {
      val maxDepth = configuration.heuristicsFoldDepth()
      fold(body)(maxDepth, hypothesis)
    } else {
      // TODO: Use hints
      val maxDepth = check.depth(hypothesis)
      fold(body)(maxDepth, hypothesis)
    }
    // exhale specification
    // TODO: Exhale existing specification
    val info = ValueInfo(instance)
    if (configuration.noInlining()) {
      val resource = instance.asResource
      emitFold(resource, info)
      emitExhale(resource)
    } else {
      emitExhale(body, info)
    }
  }

  /**
   * Saves a snapshot of the given instance.
   *
   * @param instance The instance.
   * @param exhaled  The flag indicating whether the snapshot was exhaled or not.
   */
  private def saveSnapshot(instance: Instance, exhaled: Boolean): Unit = {
    // generate unique snapshot label
    val label = namespace.uniqueIdentifier(Names.snapshot)
    query.addSnapshot(label, instance, exhaled)
    // save values of variables
    instance
      .arguments
      .foreach {
        case variable: ast.LocalVar =>
          val name = s"${label}_${variable.name}"
          emitAssignment(name, variable)
        case other =>
          sys.error(s"Unexpected argument to instance: $other")
      }
    // emit label
    emitLabel(label)
  }
}

/**
 * A partial query.
 */
private class PartialQuery {
  /**
   * The buffer used to accumulate the snapshots.
   */
  private val snapshots: mutable.Buffer[(String, Instance, Boolean)] =
    ListBuffer.empty

  /**
   * Adds a snapshot, i.e., associates the given name with the given placeholder instance.
   *
   * @param label    The label of the snapshot.
   * @param instance The instance saved by the snapshot.
   * @param exhaled  The flag indicating whether the snapshot was exhaled or not.
   */
  def addSnapshot(label: String, instance: Instance, exhaled: Boolean): Unit =
    snapshots.append((label, instance, exhaled))

  /**
   * Finalizes the query with the given program.
   *
   * @param program    The program.
   * @param hypothesis The current hypothesis.
   * @return The finalized query.
   */
  def apply(program: ast.Program, hypothesis: Hypothesis): Query =
    new Query(program, hypothesis, snapshots.toSeq)
}
