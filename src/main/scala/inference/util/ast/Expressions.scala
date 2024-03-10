/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2021 ETH Zurich.
 */

package inference.util.ast

import inference.Names
import inference.util.collections.Collections
import viper.silver.ast
import viper.silver.ast.utility.rewriter.Traverse

import scala.annotation.tailrec

object Expressions {
  /**
   * Returns the length of the given access path.
   *
   * @param path The access path.
   * @return The length.
   */
  def getLength(path: ast.Exp): Int =
    getDepth(path) + 1

  /**
   * Returns the depth of the given access path.
   *
   * @param path The access path.
   * @return The depth.
   */
  def getDepth(path: ast.Exp): Int =
    path match {
      case _: ast.NullLit => 0
      case _: ast.LocalVar => 0
      case ast.FieldAccess(receiver, _) => getDepth(receiver) + 1
      case ast.DomainFuncApp(_, Seq(receiver), _) => getDepth(receiver) + 1
      case _ => sys.error(s"Expression $path is not an access path.")
    }

  /**
   * Returns an instance of a recursive predicate segment with the given arguments.
   *
   * @param start The start argument.
   * @param stop  The stop argument.
   * @return The predicate instance.
   */
  def makeSegment(start: ast.Exp, stop: ast.Exp): ast.PredicateAccess = {
    val arguments = Seq(start, stop)
    makeRecursive(arguments)
  }

  /**
   * Returns an instance of the recursive predicate with the given arguments.
   *
   * @param arguments The arguments.
   * @return The predicate instance.
   */
  @inline
  def makeRecursive(arguments: Seq[ast.Exp]): ast.PredicateAccess =
    ast.PredicateAccess(arguments, Names.recursive)()

  /**
   * Returns a resource corresponding to the given location access.
   * @param access The location access.
   * @return The resource
   */
  def makeResource(access: ast.LocationAccess): ast.AccessPredicate =
    access match {
      case field: ast.FieldAccess => ast.FieldAccessPredicate(field, ast.FullPerm()())()
      case predicate: ast.PredicateAccess => ast.PredicateAccessPredicate(predicate, ast.FullPerm()())()
    }

  /**
   * Returns the conjunction of the given expressions.
   *
   * @param expressions The expressions to conjoin.
   * @return The conjunction.
   */
  def makeAnd(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.And(_, _)())
      .getOrElse(ast.TrueLit()())

  /**
   * Returns the disjunction of the given expressions.
   *
   * @param expressions The expressions to disjoin.
   * @return The disjunction.
   */
  def makeOr(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.Or(_, _)())
      .getOrElse(ast.FalseLit()())

  /**
   * Returns an expression which is true if at most one of the given expressions is true.
   *
   * @param expressions The expressions.
   * @return The encoding.
   */
  def makeAtMost(expressions: Iterable[ast.Exp]): ast.Exp = {
    val constraints = Collections
      .pairs(expressions)
      .map { case (left, right) => ast.Not(ast.And(left, right)())() }
    makeAnd(constraints)
  }

  /**
   * Returns an expression which is true if exactly one of the given expressions is true.
   *
   * @param expressions The expressions.
   * @return The encoding.
   */
  def makeExactly(expressions: Iterable[ast.Exp]): ast.Exp = {
    val lower = makeOr(expressions)
    val upper = makeAtMost(expressions)
    ast.And(lower, upper)()
  }

  /**
   * Returns an expression which is true if all expressions of the two given sequences are equal.
   *
   * @param first  The first sequence of expressions.
   * @param second The second sequence of expressions.
   * @return The condition.
   */
  def makeEqual(first: Seq[ast.Exp], second: Seq[ast.Exp]): ast.Exp = {
    val equalities = first
      .zip(second)
      .map { case (left, right) => ast.EqCmp(left, right)() }
    makeAnd(equalities)
  }

  /**
   * Returns the sum of the given expressions.
   *
   * @param expressions The expressions to add up.
   * @return The sum.
   */
  def makeSum(expressions: Iterable[ast.Exp]): ast.Exp =
    expressions
      .reduceOption(ast.Add(_, _)())
      .getOrElse(ast.IntLit(0)())

  /**
   * Returns a condition capturing whether there are insufficient permissions for the given access.
   *
   * @param access     The access.
   * @param permission The permission amount.
   * @return The condition.
   */
  @tailrec
  def makeInsufficient(access: ast.Exp, permission: ast.Exp = ast.FullPerm()()): ast.Exp =
    access match {
      case ast.FieldAccessPredicate(field, permission) =>
        makeInsufficient(field, permission)
      case ast.PredicateAccessPredicate(predicate, permission) =>
        makeInsufficient(predicate, permission)
      case access: ast.ResourceAccess =>
        val current = ast.CurrentPerm(access)()
        ast.PermLtCmp(current, permission)()
    }

  /**
   * Returns a condition capturing whether there are sufficient permissions for the given access.
   *
   * @param access     The access.
   * @param permission The permission amount.
   * @return The condition.
   */
  @tailrec
  def makeSufficient(access: ast.Exp, permission: ast.Exp = ast.FullPerm()()): ast.Exp =
    access match {
      case ast.FieldAccessPredicate(field, permission) =>
        makeSufficient(field, permission)
      case ast.FieldAccessPredicate(predicate, permission) =>
        makeSufficient(predicate, permission)
      case access: ast.ResourceAccess =>
        val current = ast.CurrentPerm(access)()
        ast.PermGeCmp(current, permission)()
    }

  /**
   * Simplifies the given expression.
   *
   * @param expression The expression to simplify.
   * @return The simplified expression.
   */
  def simplify(expression: ast.Exp): ast.Exp =
    expression.transform(term => simplification(term), Traverse.BottomUp)

  /**
   * Simplifies the given conjunction.
   *
   * @param conjunction The conjunction to simplify.
   * @return The simplified conjunction.
   */
  def simplifyAnd(conjunction: ast.And): ast.Exp =
    (conjunction.left, conjunction.right) match {
      case (ast.TrueLit(), right) => right
      case (left, ast.TrueLit()) => left
      case (literal: ast.FalseLit, _) => literal
      case (_, literal: ast.FalseLit) => literal
      case _ => conjunction
    }

  /**
   * Performs a simplification step.
   *
   * @param expression The expression to simplify.
   * @return The simplified expression.
   */
  private def simplification(expression: ast.Node): ast.Node =
    expression match {
      // simplify equality
      case ast.EqCmp(left, right) =>
        if (left == right) ast.TrueLit()()
        else expression
      // simplify inequality
      case ast.NeCmp(left, right) =>
        if (left == right) ast.FalseLit()()
        else expression
      // simplify negations
      case ast.Not(argument) => argument match {
        case ast.TrueLit() => ast.FalseLit()()
        case ast.FalseLit() => ast.TrueLit()()
        case ast.Not(nested) => nested
        case ast.EqCmp(left, right) => ast.NeCmp(left, right)()
        case ast.NeCmp(left, right) => ast.EqCmp(left, right)()
        case _ => expression
      }
      // simplify conjunction
      case conjunction: ast.And =>
        simplifyAnd(conjunction)
      // simplify disjunction
      case ast.Or(left, right) =>
        (left, right) match {
          case (literal: ast.TrueLit, _) => literal
          case (_, literal: ast.TrueLit) => literal
          case (ast.FalseLit(), _) => right
          case (_, ast.FalseLit()) => left
          case _ => expression
        }
      // simplify implication
      case ast.Implies(left, right) => (left, right) match {
        case (ast.TrueLit(), _) => right
        case (ast.FalseLit(), _) => ast.TrueLit()()
        case _ => expression
      }
      // do nothing by default
      case _ => expression
    }
}
