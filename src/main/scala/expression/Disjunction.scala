package expression
import context.Environment
import value.{Boole, Notification, Value}

case class Disjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    for (operand <- operands) {
      operand.execute(env) match {
        case x:Boole => if (x.value == true) return Boole(true)
      }
    }
    Boole(false)
  }
}
