/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.Names
import inference.builder.CheckExtender
import inference.core.{Hypothesis, Instance}
import inference.input._
import inference.util.ast.{Statements, ValueInfo}
import inference.util.Namespace
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A query builder mixin.
 */
trait QueryBuilder extends CheckExtender[ast.Method] {
  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

  override protected def useHints: Boolean =
    configuration.useHints()

  /**
   * The namespace used to generate unique identifiers.
   */
  private var namespace: Namespace = _

  /**
   * The partial query.
   */
  private var query: PartialQuery = _

  /**
   * Builds a query based on the given batch of checks and hypothesis.
   *
   * @param batch      The batch of checks.
   * @param hypothesis The hypothesis to check.
   * @return The query.
   */
  protected def buildQuery(batch: Seq[Check], hypothesis: Hypothesis): Query = {
    // reset
    reset()
    // get original program
    val original = input.program
    // fields
    val fields = {
      val extra = if (configuration.useHints()) Seq.empty else Seq(magic)
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
    // methods
    val methods = {
      // dummy methods fo
      val dummies = {
        val names = batch.map(_.name).toSet
        original
          .methods
          .flatMap { method =>
            if (names.contains(method.name)) None
            else {
              val dummy = method.copy(body = None)(method.pos, method.info, method.errT)
              Some(dummy)
            }
          }
      }
      // instrument methods
      implicit val current: Hypothesis = hypothesis
      val extended = batch.map(extendCheck)
      // combine dummy and instrumented methods
      dummies ++ extended
    }
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
      case MethodCheck(original, _, _, body) =>
        // instrument body
        val instrumented = {
          val extended = extendSequence(body)
          Statements.makeDeclared(extended, original.scopedDecls)
        }
        // build method based on original
        original.copy(
          pres = Seq.empty,
          posts = Seq.empty,
          body = Some(instrumented)
        )(original.pos, original.info, original.errT)
      case LoopCheck(_, name, _, body) =>
        // instrument loop
        val instrumented = {
          val extended = extendSequence(body)
          Statements.makeDeclared(extended)
        }
        // build method
        ast.Method(name, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Some(instrumented))()
    }

  override protected def processInstrumented(statement: ast.Stmt)(implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit =
    statement match {
      case ast.Seqn(statements, _) =>
        statements.foreach(processInstrumented)
      case ast.Inhale(expression) =>
        expression match {
          case ast.PredicateAccessPredicate(predicate, _) =>
            // get and inhale instance
            val instance = input.instance(predicate)
            inhaleInstance(instance)
          case condition =>
            emitInhale(condition)
        }
      case ast.Exhale(expression) =>
        expression match {
          case ast.PredicateAccessPredicate(predicate, _) =>
            // get and exhale instance
            val instance = input.instance(predicate)
            exhaleInstance(instance)
          case condition =>
            emitExhale(condition)
        }
      case other =>
        emit(other)
    }

  override protected def processCut(cut: Cut)(implicit hypothesis: Hypothesis): Unit = {
    // havoc written variables
    val written = cut.loop.original.writtenVars
    val havoc = Statements.makeHavoc(written)
    emit(havoc)
  }

  /**
   * Inhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def inhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit = {
    // inhale specification
    val body = hypothesis.getBody(instance)
    if (configuration.noInlining()) {
      val resource = instance.asResource
      emitInhale(resource)
      emitUnfold(resource)
    } else {
      emitInhale(body)
    }
    // unfold predicates appearing in specification
    unfold(body)
    // save state snapshot
    saveSnapshot(instance, exhaled = false)
  }

  /**
   * Exhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def exhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis, hints: Seq[Hint]): Unit = {
    // save state snapshot
    saveSnapshot(instance, exhaled = true)
    // fold predicates appearing in specification
    val body = hypothesis.getBody(instance)
    fold(body)
    // exhale specification
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
