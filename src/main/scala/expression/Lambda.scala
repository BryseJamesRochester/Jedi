package expression
import context.Environment
import value.{Closure, Value}

class Lambda(parameters:List[Identifier], body:Expression) extends SpecialForm {
   def execute(env: Environment): Value = {
      new Closure(env, body, parameters)
   }
}
