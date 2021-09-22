package expression
import context.Environment
import value.{Boole, Notification, Value}

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    condition.execute(env) match {
      case x:Boole =>  {if (x.value==true) consequent.execute(env)
                        else
                          alternative match {
                            case null => Notification.UNSPECIFIED()
                            case _ =>   alternative.execute(env)
                          }
                        }
      case _ => Notification.UNSPECIFIED()
    }
  }
}
