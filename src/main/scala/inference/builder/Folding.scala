package inference.builder

import inference.core.Hypothesis
import inference.input.Input
import inference.util.ast.Expressions
import viper.silver.ast

/**
 * Mixin providing methods to fold and unfold specifications.
 */
trait Folding extends Builder {
  /**
   * Returns the input.
   *
   * @return The input.
   */
  protected def input: Input

  /**
   * Unfolds the given expression up to the specified maximal depth.
   *
   * @param expression THe expression to unfold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The maximal depth.
   * @param hypothesis The current hypothesis.
   * @param default    The default action applied to leaf expressions.
   */
  protected def unfold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                      (implicit maxDepth: Int, hypothesis: Hypothesis,
                       default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        unfold(left)
        unfold(right)
      case ast.Implies(guard, guarded) =>
        unfold(guarded, guards :+ guard)
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // unfold predicate if maximal depth is not reached yet
        val depth = Expressions.getDepth(predicate.args.head)
        if (depth < maxDepth) {
          // create unfolds
          val unfolds = makeScope {
            // unfold predicate
            emitUnfold(resource)
            // recursively unfold predicates appearing in body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            unfold(body)
          }
          // conditionally unfold
          emitConditional(guards, unfolds)
        } else {
          default(resource, guards)
        }
      case other =>
        default(other, guards)
    }

  /**
   * Folds the given expression starting from the specified maximal depth.
   *
   * NOTE: The default action is used by the query builder to save permissions.
   *
   * @param expression The expression to fold.
   * @param guards     The guards collected so far.
   * @param maxDepth   The maximal depth.
   * @param hypothesis The current hypothesis.
   * @param default    THe default action applied to leaf expressions.
   */
  protected def fold(expression: ast.Exp, guards: Seq[ast.Exp] = Seq.empty)
                    (implicit maxDepth: Int, hypothesis: Hypothesis,
                     default: (ast.Exp, Seq[ast.Exp]) => Unit = (_, _) => ()): Unit =
    expression match {
      case ast.And(left, right) =>
        fold(left)
        fold(right)
      case ast.Implies(guard, guarded) =>
        fold(guarded, guards :+ guard)
      case resource@ast.PredicateAccessPredicate(predicate, _) =>
        // fold predicate if maximal depth is not reached yet
        val depth = Expressions.getDepth(predicate.args.head)
        if (depth < maxDepth) {
          // create folds
          val folds = makeScope {
            // recursively fold predicates appearing in body
            val instance = input.instance(predicate)
            val body = hypothesis.getBody(instance)
            fold(body)
            // fold predicate
            emitFold(resource)
          }
          // conditionally fold
          emitConditional(guards, folds)
        } else {
          default(resource, guards)
        }
      case other =>
        default(other, guards)
    }
}
