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
import inference.input.{Check, Configuration, Cut, Input, LoopCheck, MethodCheck}
import inference.util.ast.{Expressions, Statements, ValueInfo}
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
    // get original program and checks
    val original = input.program
    val checks = input.checks
    // predicates
    val predicates =
      if (configuration.noInlining()) {
        val existing = original.predicates
        val inferred = input
          .placeholders
          .map { placeholder => hypothesis.getPredicate(placeholder) }
        existing ++ inferred
      } else {
        original.predicates
      }
    // instrument methods
    val methods = checks.map { check => buildMethod(check)(hypothesis) }
    // instrument program
    val program = original.copy(
      predicates = predicates,
      methods = methods
    )(original.pos, original.info, original.errT)
    // finalize query
    query(program)
  }

  /**
   * Resets the query builder.
   */
  private def reset(): Unit = {
    namespace = new Namespace()
    query = new PartialQuery
  }

  /**
   * Builds a method corresponding to the given check.
   *
   * @param check      The check.
   * @param hypothesis The current hypothesis.
   * @return The built method.
   */
  private def buildMethod(check: Check)(implicit hypothesis: Hypothesis): ast.Method =
    check match {
      case MethodCheck(original, precondition, postcondition, body) =>
        // instrument method
        val instrumented = makeScope {
          inhaleInstance(precondition.asInstance)
          instrumentStatement(body)
          exhaleInstance(postcondition.asInstance)
        }
        // build method based on original
        original.copy(
          pres = Seq.empty,
          posts = Seq.empty,
          body = Some(instrumented)
        )(original.pos, original.info, original.errT)
      case check@LoopCheck(original, name, invariant, body) =>
        // instrument loop
        val instrumented = {
          val sequence = makeScope {
            inhaleInstance(invariant.asInstance)
            emitInhale(check.condition)
            instrumentStatement(body)
            exhaleInstance(invariant.asInstance)
          }
          val declarations = sequence
            .undeclLocalVars
            .map { x => ast.LocalVarDecl(x.name, x.typ)() }
          sequence.copy(scopedDecls = declarations)(sequence.pos, sequence.info, sequence.errT)
        }
        // build method
        ast.Method(name, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Some(instrumented))()
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
          .placeholder(name)
          .asInstance(arguments)
        inhaleInstance(instance)
      case ast.Exhale(ast.PredicateAccessPredicate(ast.PredicateAccess(arguments, name), _)) =>
        // get  and exhale instance
        val instance = input
          .placeholder(name)
          .asInstance(arguments)
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
        val havoc = Statements.havoc(loop.original.writtenVars)
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
    val body = hypothesis.getBody(instance)
    // save and fold
    implicit val label: String = saveSnapshot(instance)
    fold(body)(maxDepth = 0, hypothesis, savePermission)
    // exhale specification
    // TODO: Exhale existing specification
    val info = ValueInfo(instance)
    if (configuration.noInlining()) {
      val resource = instance.asResource()
      emitFold(resource, info)
      emitExhale(resource)
    } else {
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
