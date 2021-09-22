package expression
import context.{Environment, UndefinedException}
import value.{Notification, Value, Variable}

class Assignment(vbl:Identifier, update:Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (env.contains(vbl)) env(vbl).asInstanceOf[Variable].content = update.execute(env)
    else throw new UndefinedException(vbl)
    Notification.DONE()
  }
}
