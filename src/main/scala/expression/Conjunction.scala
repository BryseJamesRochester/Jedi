package expression

import context.Environment
import value.{Boole, Value}

case class Conjunction (operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    for (operand <- operands) {
      operand.execute(env) match {
        case x:Boole => if (x.value == false) return Boole(false)
      }
    }
    Boole(true)
  }
}
