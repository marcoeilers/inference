/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.learner

import inference.core.{Placeholder, Sample}
import inference.runner.Input
import inference.util.collections.SetMap
import viper.silver.ast

import java.util.concurrent.atomic.AtomicInteger

/**
 * A template generator mixin.
 */
trait TemplateGenerator {
  /**
   * Returns the input to the inference.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Generates templates corresponding to the given sequence of samples.
   *
   * @param samples The samples.
   * @return The templates.
   */
  protected def generateTemplates(samples: Seq[Sample]): Seq[Template] = {
    // counter used to generate unique ids
    implicit val id: AtomicInteger = new AtomicInteger()

    // map from placeholder names to locations
    val map = {
      val empty: Map[String, Set[ast.LocationAccess]] = Map.empty
      samples
        .flatMap(_.records)
        .foldLeft(empty) {
          case (result, record) =>
            val name = record.placeholder.name
            val locations = record.locations
            SetMap.addAll(result, name, locations)
        }
    }

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
      val fields = locations.collect { case field: ast.FieldAccess => field }
      val conjuncts = createResources(fields)
      Conjunction(conjuncts)
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
  private def createResources(fields: Set[ast.FieldAccess])(implicit id: AtomicInteger): Seq[TemplateExpression] = {
    // TODO: Sort fields.
    val sorted = fields.toSeq
    // crate template expression
    sorted.map(createGuarded)
  }

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
}
