package expression
import context.Environment
import value.{Notification, Value}

case class Declaration(identifier:Identifier, expression:Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    env(identifier) = expression.execute(env)
    Notification.OK
  }

}
