package inference.extender

import inference.core.Hypothesis
import inference.runner.Input
import viper.silver.ast

/**
 * An extender.
 */
trait Extender {
  /**
   * Extends the program given by the input with specifications corresponding to the given hpyothesis.
   *
   * @param input      The input to the inference.
   * @param hypothesis The inferred hypothesis.
   * @return The extended program.
   */
  def extend(input: Input, hypothesis: Hypothesis): ast.Program = {
    // TODO: Implement me.
    input.program
  }
}
