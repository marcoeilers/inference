package inference.util.ast

import viper.silver.ast

object Infos {
  /**
   * Returns the info value attached to the given node.
   *
   * @param node The node.
   * @tparam T The type of the value.
   * @return The value.
   */
  def value[T](node: ast.Infoed): T =
    valueOption(node) match {
      case Some(value) => value
      case None => sys.error("No info value defined.")
    }

  /**
   * Optionally returns the info value attached to the given node.
   *
   * @param node The node.
   * @tparam T The type of the value.
   * @return The value.
   */
  def valueOption[T](node: ast.Infoed): Option[T] =
    node
      .info
      .getUniqueInfo[ValueInfo[T]]
      .map(_.value)
}

trait InferenceInfo extends ast.Info {
  override def comment: Seq[String] =
    Seq.empty

  override def isCached: Boolean =
    true
}

/**
 * An info holding a value of some type.
 *
 * @param value The value.
 * @tparam T The type of the value.
 */
case class ValueInfo[+T](value: T) extends InferenceInfo

/**
 * A mixin that enables comments.
 */
trait Comment extends ValueInfo[Any] {
  override def comment: Seq[String] =
    Seq(value.toString)
}