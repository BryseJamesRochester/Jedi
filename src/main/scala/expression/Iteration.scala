package expression
import context.Environment
import value.{Boole, Notification, Value}

class Iteration(condition:Expression, body:Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    while(condition.execute(env) == Boole(true)) body.execute(env)
    Notification.DONE()
  }
}
