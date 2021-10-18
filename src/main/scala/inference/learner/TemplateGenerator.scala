/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core.sample.Sample
import inference.core.{AbstractLearner, Placeholder}
import inference.input.{Configuration, Input}
import inference.util.ast.Expressions
import inference.util.collections.SetMap
import viper.silver.ast

import java.util.concurrent.atomic.AtomicInteger

/**
 * A template generator mixin.
 */
trait TemplateGenerator extends AbstractLearner {
  /**
   * The map from placeholder names to location accesses.
   */
  private var map: Map[String, Set[ast.LocationAccess]] =
    Map.empty

  override def addSample(sample: Sample): Unit = {
    super.addSample(sample)
    sample
      .records
      .foreach { record =>
        val placeholder = record.placeholder
        record
          .resource
          .locations
          .foreach { location => addLocation(placeholder, location) }
      }
  }

  /**
   * Processes and adds the given location to the given specification placeholder.
   *
   * @param placeholder The placeholder.
   * @param location    The location access to add.
   */
  private def addLocation(placeholder: Placeholder, location: ast.LocationAccess): Unit = {
    /**
     * Helper method that conditionally adds the given location access to the given specification placeholder.
     *
     * @param placeholder The specification placeholder.
     * @param location    The location access to add.
     */
    def addIfAllowed(placeholder: Placeholder, location: ast.LocationAccess): Unit = {
      // filter location access
      val allowed = location match {
        case path: ast.FieldAccess =>
          Expressions.getLength(path) <= configuration.maxLength
        case ast.PredicateAccess(arguments, _) =>
          arguments.zipWithIndex.forall {
            case (_: ast.NullLit, index) => index > 0
            case (_: ast.LocalVar, _) => true
            case _ => false
          }
      }
      if (allowed) add(placeholder, location)
    }

    /**
     * Helper method that adds the given location access to the given specification placeholder.
     *
     * @param placeholder The specification placeholder.
     * @param location    The location access to add.
     */
    def add(placeholder: Placeholder, location: ast.LocationAccess): Unit = {
      val name = placeholder.name
      map = SetMap.add(map, name, location)
    }

    // process location access
    location match {
      case ast.FieldAccess(receiver, field) =>
        receiver match {
          case nested@ast.FieldAccess(root, next) =>
            // add nested location
            addLocation(placeholder, nested)
            // add potential recursions
            if (configuration.useRecursive) {
              // get parameters of recursive predicate
              val recursive = input.placeholder(Names.recursive)
              val from +: rest = recursive.variables
              // add field access to recursive predicate
              val access = ast.FieldAccess(from, field)()
              add(recursive, access)
              // add recursion to recursive predicate
              val recursion = Expressions.makeRecursive(ast.FieldAccess(from, next)() +: rest)
              add(recursive, recursion)
              // add instance to current specification
              val instance = makeInstance(root)
              addIfAllowed(placeholder, instance)
            }
          case _ => // do nothing
        }
      case ast.PredicateAccess(first +: rest, name) =>
        assert(Names.isRecursive(name))
        first match {
          case ast.FieldAccess(receiver, _) if !placeholder.isRecursive =>
            // add parent predicate
            val parent = Expressions.makeRecursive(receiver +: rest)
            addLocation(placeholder, parent)
          case _ => // do nothing
        }
    }

    // add location access
    addIfAllowed(placeholder, location)
  }

  /**
   * Generates templates corresponding to the currently accumulated samples.
   *
   * @return The templates.
   */
  protected def generateTemplates(): Seq[Template] = {
    // counter used to generate unique ids
    implicit val id: AtomicInteger = new AtomicInteger()

    // generate templates
    map
      .toSeq
      .flatMap { case (name, resources) =>
        val placeholder = input.placeholder(name)
        generateTemplate(placeholder, resources)
      }
  }

  /**
   * Generates a template for the given specification placeholder with the given locations.
   *
   * @param placeholder The specification placeholder.
   * @param locations   The locations.
   * @param id          The implicitly passed counter used to generate unique ids.
   * @return The template.
   */
  private def generateTemplate(placeholder: Placeholder, locations: Set[ast.LocationAccess])(implicit id: AtomicInteger): Seq[Template] = {
    // create template body
    val body = {
      // create resources for field access
      val fields = {
        val accesses = locations.collect { case field: ast.FieldAccess => field }
        createFieldResources(accesses)
      }
      // create resources for all predicate access
      val predicates = {
        val accesses = locations.collect { case predicate: ast.PredicateAccess => predicate }
        createPredicateResources(placeholder, accesses)
      }
      Conjunction(fields ++ predicates)
    }
    // create template
    if (placeholder.isRecursive && configuration.useSegments) {
      // create template for recursive predicate
      val predicate = {
        val Seq(start, stop) = placeholder.variables
        val condition = ast.NeCmp(start, stop)()
        val truncated = Truncated(condition, body)
        PredicateTemplate(placeholder, truncated)
      }
      // create template for lemmas
      val appendLemma = createAppendLemma(predicate)
      val concatLemma = createConcatLemma(predicate)
      // return all templates
      Seq(predicate, appendLemma, concatLemma)
    } else {
      // create and return template
      val predicate = PredicateTemplate(placeholder, body)
      Seq(predicate)
    }
  }

  /**
   * Creates template expressions for the given field accesses.
   *
   * @param fields The field accesses.
   * @param id     The implicitly passed counter used to generate unique ids.
   * @return The template expressions.
   */
  private def createFieldResources(fields: Set[ast.FieldAccess])(implicit id: AtomicInteger): Seq[TemplateExpression] = {
    // sort fields
    val sorted =
      if (configuration.maxLength <= 2) fields.toSeq
      else ???
    // crate template expression
    sorted.map(createGuarded)
  }

  /**
   * Creates template expressions for the given specification placeholder and predicate accesses.
   *
   * @param placeholder The specification placeholder.
   * @param predicates  The predicate accesses.
   * @param id          The implicitly passed counter used to generate unique ids.
   * @return The template expressions.
   */
  private def createPredicateResources(placeholder: Placeholder, predicates: Set[ast.PredicateAccess])
                                      (implicit id: AtomicInteger): Seq[TemplateExpression] = {
    if (configuration.useSegments) {
      // group predicate instances by their start argument and introduce choices for all possible end arguments
      val choice =
        if (configuration.choiceIntroduction && !placeholder.isRecursive) {
          val options = {
            val variables = placeholder.variables
            val references = variables.filter(_.isSubtype(ast.Ref))
            references :+ ast.NullLit()()
          }
          predicates.map { predicate =>
            val start = predicate.args.head
            val end = options.filter(_ != start)
            start -> end
          }
        } else {
          predicates.foldLeft(Map.empty[ast.Exp, Set[ast.Exp]]) {
            case (map, predicate) =>
              assert(Names.isRecursive(predicate.predicateName))
              val Seq(start, end) = predicate.args
              SetMap.add(map, start, end)
          }
        }
      // process choices
      choice
        .map { case (start, options) =>
          if (options.size == 1) {
            // no choice needed if there is only one option
            val predicate = Expressions.makeSegment(start, options.head)
            createGuarded(predicate)
          } else {
            // introduce choice
            val choiceId = id.getAndIncrement()
            val variable = ast.LocalVar(s"c_$choiceId", ast.Ref)()
            val predicate = Expressions.makeSegment(start, variable)
            val body = createGuarded(predicate)
            Choice(choiceId, variable, options.toSeq, body)
          }
        }
        .toSeq
    } else {
      predicates
        .toSeq
        .map(createGuarded)
    }
  }

  /**
   * Creates a template for the append lemma based on the given template for the recursive predicate.
   *
   * @param template The template for the recursive predicate.
   * @return The template for the append lemma.
   */
  private def createAppendLemma(template: PredicateTemplate): Template = {
    // get placeholder
    val placeholder = input.placeholder(Names.appendLemma)
    val Seq(start, stop, next) = placeholder.variables
    val instance = placeholder.asInstance(Seq(stop, next))

    /**
     * Helper method that processes the given template expression.
     *
     * @param expression The template expression to process.
     * @return The processed template expression.
     */
    def process(expression: TemplateExpression): TemplateExpression =
      expression match {
        case Wrapped(expression) =>
          expression match {
            case ast.PredicateAccessPredicate(predicate, _) =>
              val Seq(recursion, _) = predicate.args
              val instantiated = instance.instantiate(recursion)
              val equality = ast.EqCmp(instantiated, next)()
              Wrapped(equality)
            case other =>
              val instantiated = instance.instantiate(other)
              Wrapped(instantiated)
          }
        case Conjunction(conjuncts) =>
          val processed = conjuncts.map(process)
          Conjunction(processed)
        case Guarded(guardId, body) =>
          val processed = process(body)
          Guarded(guardId, processed)
        case Choice(choiceId, variable, options, body) =>
          val processed = process(body)
          Choice(choiceId, variable, options, processed)
        case Truncated(condition, body) =>
          val instantiated = instance.instantiate(condition)
          val processed = process(body)
          Truncated(instantiated, processed)
      }

    // create template expression for lemma precondition
    val precondition = {
      val segment = Expressions.makeSegment(start, stop)
      val wrapped = wrap(segment)
      val link = process(template.body)
      val conjuncts = Seq(wrapped, link)
      Conjunction(conjuncts)
    }
    // create template expression for lemma postcondition
    val postcondition = {
      val segment = Expressions.makeSegment(start, next)
      wrap(segment)
    }
    // create lemma template
    LemmaTemplate(placeholder, precondition, postcondition)
  }

  /**
   * Creates a template for the concat lemma based on the given template for the recursive predicate.
   *
   * @param template The template for the recursive predicate.
   * @return The template for the concat lemma.
   */
  private def createConcatLemma(template: PredicateTemplate): Template = {
    // get placeholder and parameters
    val placeholder = input.placeholder(Names.concatLemma)
    val Seq(start, middle, end) = placeholder.variables
    // create template expression for lemma precondition
    val precondition = {
      val first = Expressions.makeSegment(start, middle)
      val second = Expressions.makeSegment(middle, end)
      val conjuncts = Seq(wrap(first), wrap(second))
      Conjunction(conjuncts)
    }
    // create template expression for lemma postcondition
    val postcondition = {
      val segment = Expressions.makeSegment(start, end)
      wrap(segment)
    }
    // create lemma template
    LemmaTemplate(placeholder, precondition, postcondition)
  }

  /**
   * Creates a template expression that guards the resource corresponding to the given location.
   *
   * @param location The location.
   * @param id       The implicitly passed counter used to generate unique ids.
   * @return The template expression.
   */
  private def createGuarded(location: ast.LocationAccess)(implicit id: AtomicInteger): TemplateExpression = {
    // wrap and guard resource
    val guardId = id.getAndIncrement()
    val wrapped = wrap(location)
    Guarded(guardId, wrapped)
  }

  /**
   * Wraps the given expression as a template expression.
   *
   * @param expression The expression to wrap.
   * @return The template expression.
   */
  private def wrap(expression: ast.Exp): TemplateExpression = {
    // process expression by making sure accesses are turned into resources
    val processed = expression match {
      case access: ast.LocationAccess => Expressions.makeResource(access)
      case other => other
    }
    // wrap processed expression
    Wrapped(processed)
  }

  /**
   * Returns an instance of the recursive predicate rooted at the given expression.
   *
   * @param root The root.
   * @return The predicate instance.
   */
  private def makeInstance(root: ast.Exp): ast.PredicateAccess = {
    val arguments = if (configuration.useSegments) Seq(root, ast.NullLit()()) else Seq(root)
    Expressions.makeRecursive(arguments)
  }
}
