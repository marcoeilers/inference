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
import inference.util.ast.{Expressions, InstanceInfo, LocationInfo, Statements}
import inference.util.Namespace
import inference.util.collections.Collections
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

  override protected val exhale: Boolean =
    true

  /**
   * The namespace used to generate unique identifiers.
   */
  private var namespace: Namespace = _

  /**
   * The map used to collect leaf access predicates, i.e., the access predicates present in the state after unfolding
   * the specification at hand. Each access predicate maps to the condition under which it is present in the state.
   */
  private var leaves: Map[ast.AccessPredicate, ast.Exp] = _

  /**
   * Clears all leaves.
   */
  private def clearLeaves(): Unit =
    leaves = Map.empty

  /**
   * Adds the given leaf expression if it is an access predicate.
   *
   * @param leaf   The leaf to add.
   * @param guards The guards.
   */
  private def addLeaf(leaf: ast.Exp, guards: Seq[ast.Exp]): Unit =
    leaf match {
      case leaf: ast.AccessPredicate =>
        // combine existing condition with guards
        val condition = Expressions.makeAnd(guards)
        val combined = leaves
          .get(leaf)
          .map { existing => ast.Or(existing, condition)() }
          .getOrElse(condition)
        // update laves
        leaves = leaves.updated(leaf, combined)
      case _ => // do nothing
    }

  /**
   * The partial query.
   */
  private var query: PartialQuery = _

  /**
   * Builds a query that checks whether the specifications represented by the given hypothesis are self-framing.
   *
   * @param hypothesis The hypothesis to check.
   * @return The framing query.
   */
  protected def framingQuery(hypothesis: Hypothesis): Query = {
    /**
     * Helper method that inhales the given expression conjunct-wise. The expression is implicitly rewritten to have
     * its conjuncts at the top level by pushing implications inside.
     *
     * @param expression The expression to inhale.
     * @param guards     The guards collected so far.
     */
    def inhale(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty): Unit =
      expression match {
        case ast.TrueLit() => // do nothing
        case ast.And(left, right) =>
          inhale(left, guards)
          inhale(right, guards)
        case ast.Implies(guard, guarded) =>
          inhale(guarded, guards :+ guard)
        case conjunct =>
          // create info carrying the location
          val info = conjunct match {
            case ast.FieldAccessPredicate(location, _) =>
              LocationInfo(location)
            case ast.PredicateAccessPredicate(location, _) =>
              LocationInfo(location)
            case other =>
              sys.error(s"Unexpected conjunct: $other")
          }
          // inhale conjunct
          val condition = Expressions.makeAnd(guards)
          val implication = ast.Implies(condition, conjunct)()
          emitInhale(implication, info)
      }

    // reset
    reset()

    // create predicates (dummy for recursive predicate)
    val predicates = hypothesis
      .predicates
      .flatMap { predicate =>
        if (Names.isRecursive(predicate.name)) {
          val dummy = predicate.copy(body = None)(predicate.pos, predicate.info, predicate.errT)
          Some(dummy)
        } else None
      }

    // create methods (one for each specification)
    val methods = hypothesis
      .predicates
      .map { case ast.Predicate(name, arguments, Some(specification)) =>
        // create method inhaling the specification
        val unique = namespace.uniqueIdentifier(name = s"check_$name", None)
        val body = makeScope {
          // save state snapshot
          val instance = input.placeholder(name).asInstance
          saveSnapshot(instance)
          // inhale specification
          inhale(specification)
        }
        ast.Method(unique, arguments, Seq.empty, Seq.empty, Seq.empty, Some(body))()
      }

    // create program
    val original = input.program
    val program = original.copy(
      predicates = predicates,
      methods = methods
    )(original.pos, original.info, original.errT)

    // finalize query
    query(program, hypothesis)
  }

  /**
   * Builds a query based on the given batch of checks and hypothesis.
   *
   * @param batch      The batch of checks.
   * @param hypothesis The hypothesis to check.
   * @return The query.
   */
  protected def basicQuery(batch: Seq[Check], hypothesis: Hypothesis): Query = {
    // reset and get original program
    reset()
    val original = input.program

    // create predicates
    val predicates =
      input.placeholders.flatMap { placeholder =>
        if (placeholder.isPredicate) {
          val predicate = hypothesis.getPredicate(placeholder)
          Some(predicate)
        } else {
          None
        }
      }

    // create methods
    val methods = {
      // dummy methods
      val dummies = {
        // state consolidation method
        val consolidate =
          if (configuration.stateConsolidation) Seq(consolidateMethod)
          else Seq.empty
        // methods for method not contained in batch
        val names = batch.map(_.name).toSet
        val unchecked = original
          .methods
          .flatMap { method =>
            if (names.contains(method.name)) None
            else {
              val dummy = method.copy(body = None)(method.pos, method.info, method.errT)
              Some(dummy)
            }
          }
        // concatenate all dummy methods
        consolidate ++ unchecked
      }
      // lemma methods
      val lemmas = hypothesis.lemmas
      // instrument methods
      implicit val current: Hypothesis = hypothesis
      val extended = batch.map(extendCheck)
      // combine lemma, dummy and instrumented methods
      dummies ++ lemmas ++ extended
    }

    // create program
    val program = original.copy(
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

  override protected def processInstrumented(statement: ast.Stmt)(implicit hypothesis: Hypothesis, annotations: Seq[Annotation]): Unit =
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
  private def inhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis, hints: Seq[Annotation]): Unit = {
    // get body of instance
    val body = hypothesis.getBody(instance)
    // inhale specification
    val inhales = commented(instance.toString) {
      emitInhale(body)
    }
    emit(inhales)
    // unfold predicates appearing in specification
    clearLeaves()
    unfold(body, configuration.querySimplification)(hypothesis, addLeaf)
    // branch on accesses
    if (configuration.useBranching) {
      branch(instance)
    }
    // consolidate state if enabled
    if (configuration.stateConsolidation) {
      consolidateState()
    }
    // save state snapshot
    saveSnapshot(instance)
  }

  /**
   * Exhales the given specification instance.
   *
   * @param instance   The instance.
   * @param hypothesis The implicitly passed current hypothesis.
   */
  private def exhaleInstance(instance: Instance)(implicit hypothesis: Hypothesis, hints: Seq[Annotation]): Unit = {
    // get body of instance
    val body = hypothesis.getBody(instance)
    // save state snapshot
    saveSnapshot(instance, exhaled = true)
    // exhale specification
    val exhales = commented(instance.toString) {
      implicit val info: ast.Info = InstanceInfo(instance)
      // actually exhales the body since the corresponding flag is set appropriately
      fold(body, configuration.querySimplification)
    }
    emit(exhales)
  }

  /**
   * Saves a snapshot of the given instance.
   *
   * @param instance The instance.
   * @param exhaled  The flag indicating whether the snapshot was exhaled or not.
   */
  private def saveSnapshot(instance: Instance, exhaled: Boolean = false): Unit = {
    // generate unique snapshot label
    val label = namespace.uniqueIdentifier(Names.snapshot)
    query.addSnapshot(label, instance, exhaled)
    // save values of variables
    instance
      .arguments
      .foreach { argument =>
        if (argument.isSubtype(ast.Ref)) {
          argument match {
            case literal: ast.NullLit =>
              literal
            case variable: ast.LocalVar =>
              val name = s"${label}_${variable.name}"
              emitAssignment(name, variable)
            case other =>
              sys.error(s"Unexpected argument to instance: $other")
          }
        }
      }
    // emit label
    emitLabel(label)
  }

  /**
   * Branches on atomic predicates that can be formed from the accesses appearing in the given instance and collected
   * leaves.
   *
   * @param instance The instance.
   */
  private def branch(instance: Instance): Unit = {
    // collected accesses
    val accesses = {
      // variables appearing in instance
      val variables = instance
        .arguments
        .filter(_.isSubtype(ast.Ref))
        .map { argument => argument -> ast.TrueLit()() }
      // collected field accesses
      val fields = leaves.collect {
        case (ast.FieldAccessPredicate(access, _), condition) if access.isSubtype(ast.Ref) =>
          access -> condition
      }
      // combine variables and fields
      variables ++ fields
    }
    // dummy statement
    val dummy = makeScope(emitInhale(ast.TrueLit()()))
    // branch on nullity
    accesses.foreach {
      case (access, effective) =>
        val atom = ast.NeCmp(access, ast.NullLit()())()
        val condition = ast.And(effective, atom)()
        emitConditional(condition, dummy)
    }
    // branch on equality
    Collections.pairs(accesses).foreach {
      case ((access1, effective1), (access2, effective2)) =>
        val atom = ast.NeCmp(access1, access2)()
        val condition = Expressions.makeAnd(Seq(effective1, effective2, atom))
        emitConditional(condition, dummy)
    }
  }

  /**
   * Consolidates the state.
   */
  private def consolidateState(): Unit = {
    // collect leaf predicates
    val predicates = leaves.collect {
      case (predicate: ast.PredicateAccessPredicate, condition) =>
        predicate -> condition
    }
    // unfold leaf predicates
    predicates.foreach {
      case (predicate, condition) =>
        val unfold = ast.Unfold(predicate)()
        emitConditional(condition, unfold)
    }
    // consolidate state
    emitConsolidate()
    // fold leaf predicates
    predicates.foreach {
      case (predicate, condition) =>
        val fold = ast.Fold(predicate)()
        emitConditional(condition, fold)
    }
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
