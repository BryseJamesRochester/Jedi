package expression

import value.{Closure, Thunk, Value}
import context.{Environment, alu, flags}

case class FunCall(operator:Identifier, operands:List[Expression]) extends Expression{

  def execute(env:Environment) = {
    var arguments:List[Value] = Nil
    if (env.contains(operator)) {
      if (flags.paramPassing == flags.BY_NAME) {
        arguments = operands.map(new Thunk(_, env)).asInstanceOf[List[Value]]
      }else {
        arguments = operands.map(_.execute(env))
      }
      env(operator).asInstanceOf[Closure].apply(arguments)
    } else{
      arguments = operands.map(_.execute(env))
      alu.execute(operator, arguments)
    }
  }

}
