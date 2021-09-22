package expression

import context.Environment
import value.{Exact, Value}

case class Block(expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val tempEnv: Environment = new Environment(env)
    var res: Value = Exact(0)
    for (expression <- expressions) res = expression.execute(tempEnv)
    res
  }
}
