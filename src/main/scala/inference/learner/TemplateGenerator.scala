/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.Names
import inference.core.{AbstractLearner, Placeholder, Sample}
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
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  private var map: Map[String, Set[ast.LocationAccess]] =
    Map.empty

  /**
   * Returns the configuration.
   *
   * @return The configuration.
   */
  private def configuration: Configuration =
    input.configuration

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
          Expressions.getLength(path) <= configuration.maxLength()
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
            if (configuration.useRecursion()) {
              // get parameters of recursive predicate
              val recursive = input.placeholder(Names.recursive)
              val from +: rest = recursive.variables
              // add field access to recursive predicate
              val access = ast.FieldAccess(from, field)()
              add(recursive, access)
              // add recursion to recursive predicate
              val recursion = makeRecursive(ast.FieldAccess(from, next)() +: rest)
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
            val parent = makeRecursive(receiver +: rest)
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
      .map { case (name, resources) =>
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
  private def generateTemplate(placeholder: Placeholder, locations: Set[ast.LocationAccess])(implicit id: AtomicInteger): Template = {
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
        createPredicateResources(accesses)
      }
      Conjunction(fields ++ predicates)
    }
    // create template
    PredicateTemplate(placeholder, body)
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
      if (configuration.maxLength() <= 2) fields.toSeq
      else ???
    // crate template expression
    sorted.map(createGuarded)
  }

  /**
   * Creates template expressions for the given predicate accesses
   *
   * @param predicates The predicate accesses.
   * @param id         The implicitly passed counter used to generate unique ids.
   * @return The template expressions.
   */
  private def createPredicateResources(predicates: Set[ast.PredicateAccess])(implicit id: AtomicInteger): Seq[TemplateExpression] =
    predicates
      .toSeq
      .map(createGuarded)

  /**
   * Creates a template expression that guards the resource corresponding to the given location.
   *
   * @param location The location.
   * @param id       The implicitly passed counter used to generate unique ids.
   * @return The template expression.
   */
  private def createGuarded(location: ast.LocationAccess)(implicit id: AtomicInteger): TemplateExpression = {
    // create resource
    val resource = location match {
      case field: ast.FieldAccess => ast.FieldAccessPredicate(field, ast.FullPerm()())()
      case predicate: ast.PredicateAccess => ast.PredicateAccessPredicate(predicate, ast.FullPerm()())()
    }
    // wrap and guard resource
    val guardId = id.getAndIncrement()
    val wrapped = Wrapped(resource)
    Guarded(guardId, wrapped)
  }

  /**
   * Returns an instance of the recursive predicate rooted at the given expression.
   *
   * @param root The root.
   * @return The predicate instance.
   */
  private def makeInstance(root: ast.Exp): ast.PredicateAccess = {
    val arguments = if (configuration.useSegments()) Seq(root, ast.NullLit()()) else Seq(root)
    makeRecursive(arguments)
  }

  /**
   * Returns an instance of the recursive predicate with the given arguments.
   *
   * @param arguments The arguments.
   * @return The predicate instance.
   */
  @inline
  private def makeRecursive(arguments: Seq[ast.Exp]): ast.PredicateAccess =
    ast.PredicateAccess(arguments, Names.recursive)()
}
