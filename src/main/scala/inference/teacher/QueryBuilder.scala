/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.teacher

import inference.Names
import inference.builder.{Builder, Folding}
import inference.core.{Hypothesis, Instance}
import inference.runner.{Configuration, Input}
import inference.util.ast.{Expressions, ValueInfo}
import inference.util.Namespace
import viper.silver.ast

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A query builder mixin.
 */
trait QueryBuilder extends Builder with Folding {
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
          // inhale method precondition
          method.pres.foreach { expression =>
            val inhale = ast.Inhale(expression)()
            instrumentStatement(inhale)
          }
          // instrument method body
          instrumentStatement(body)
          // exhale method postcondition
          method.posts.foreach { expression =>
            val exhale = ast.Exhale(expression)()
            instrumentStatement(exhale)
          }
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
        // get and inhale instance
        val instance = input
          .placeholders(name)
          .asInstance(arguments)
        inhaleInstance(instance)
      case ast.Exhale(ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _)) =>
        // get  and exhale instance
        val instance = input
          .placeholders(name)
          .asInstance(arguments)
        exhaleInstance(instance)
      case call@ast.MethodCall(name, arguments, _) =>
        // get specification placeholders
        val (precondition, postcondition) = input.methods(name)
        // exhale method precondition (method's precondition was replaced with true)
        exhaleInstance(precondition.asInstance(arguments))
        // emit method call (to havoc targets)
        emit(call)
        // inhale method postcondition (method's postcondition was replaced with true)
        inhaleInstance(postcondition.asInstance(arguments))
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
    val body = hypothesis.get(instance)
    // inhale specification
    // TODO: Inhale existing specification
    if (configuration.noInlining()) {
      val resource = instance.asResource()
      emitInhale(resource)
      emitUnfold(resource)
    } else {
      emitInhale(body)
    }
    // unfold and save
    unfold(body)(maxDepth = 0, hypothesis)
    saveSnapshot(instance)
  }

  /**
   * Exhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def exhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis): Unit = {
    // get body
    val body = hypothesis.get(instance)
    // save and fold
    implicit val label: String = saveSnapshot(instance)
    fold(body)(maxDepth = 0, hypothesis, savePermission)
    // exhale specification
    // TODO: Exhale existing specification
    if (configuration.noInlining()) {
      val resource = instance.asResource()
      emitFold(resource)
      emitExhale(resource)
    } else {
      val info = ValueInfo(instance)
      emitExhale(body, info)
    }
  }

  /**
   * Saves the permission value for the given expression if it a resource predicate.
   *
   * @param expression The expression.
   * @param guards     The guards guarding the expression.
   * @param label      The implicitly passed label of the current state snapshot.
   */
  private def savePermission(expression: ast.Exp, guards: Seq[ast.Exp])(implicit label: String): Unit =
    expression match {
      case ast.FieldAccessPredicate(access, _) =>
        savePermission(access, guards)
      case ast.PredicateAccessPredicate(access, _) =>
        savePermission(access, guards)
      case _ => // do nothing
    }

  /**
   * Saves the permission value for the given access.
   *
   * @param access The access.
   * @param guards The guards guarding the access.
   * @param label  The implicitly passed label of the current state snapshot.
   */
  private def savePermission(access: ast.LocationAccess, guards: Seq[ast.Exp])(implicit label: String): Unit = {
    /**
     * Helper method that extracts the condition under which we have the permissions to talk about the given expression.
     *
     * @param expression The expression.
     * @return The condition.
     */
    def extractCondition(expression: ast.Exp): Seq[ast.Exp] =
      expression match {
        case access: ast.FieldAccess =>
          val name = query.name(label, access)
          val variable = ast.LocalVar(name, ast.Perm)()
          val comparison = ast.PermGtCmp(variable, ast.NoPerm()())()
          Seq(comparison)
        case _ =>
          Seq.empty
      }

    // generate unique name
    val name = namespace.uniqueIdentifier(Names.permission)
    query.addName(label, name, access)

    val condition = {
      // compute conditions under which we have the permissions to talk about the given access
      val conditions = access match {
        case ast.FieldAccess(receiver, _) =>
          extractCondition(receiver)
        case ast.PredicateAccess(arguments, _) =>
          arguments.flatMap(extractCondition)
      }
      // conjoin given guards and these conditions
      Expressions.conjoin(guards ++ conditions)
    }
    val value = ast.CondExp(condition, ast.CurrentPerm(access)(), ast.NoPerm()())()
    emitAssignment(name, value)
  }

  /**
   * Saves a snapshot of the given instance.
   *
   * @param instance The instance.
   * @return The label of the state snapshot.
   */
  private def saveSnapshot(instance: Instance): String = {
    // generate unique snapshot label
    val label = namespace.uniqueIdentifier(Names.snapshot)
    query.addSnapshot(label, instance)
    // emit and return label
    emitLabel(label)
    label
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
   * The map used to remember the names of permission variables.
   */
  private var names: Map[String, Map[ast.Exp, String]] =
    Map.empty

  /**
   * Adds a snapshot, i.e., associates the given name with the given placeholder instance.
   *
   * @param label    The label of the snapshot.
   * @param instance The instance saved by the snapshot.
   */
  def addSnapshot(label: String, instance: Instance): Unit =
    snapshots.append(label -> instance)

  /**
   * Remembers the variable name storing the permission value for the given expression in the state snapshot with the
   * given label.
   *
   * @param label      The label of the state snapshot.
   * @param name       The name of the permission variable.
   * @param expression The expression.
   */
  def addName(label: String, name: String, expression: ast.Exp): Unit = {
    val added = names
      .getOrElse(label, Map.empty)
      .updated(expression, name)
    names = names.updated(label, added)
  }

  /**
   * @see [[Query.name]]
   */
  def name(label: String, expression: ast.Exp): String =
    names(label)(expression)

  /**
   * Finalizes the query with the given program.
   *
   * @param program The program.
   * @return The finalized query.
   */
  def apply(program: ast.Program): Query =
    Query(program, snapshots.toSeq, names)
}
