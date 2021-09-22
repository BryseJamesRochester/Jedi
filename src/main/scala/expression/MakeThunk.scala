package expression
import context.Environment
import value.{Thunk, Value}

class MakeThunk(body:Expression) extends SpecialForm {
  def execute(env: Environment): Value = new Thunk(body, env)
}
