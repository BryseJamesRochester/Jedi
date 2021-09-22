package value

import context.{Environment, TypeException}
import expression.{Expression, Identifier}

class Closure(defEnv:Environment, body:Expression, parameters:List[Identifier]) extends Value{
  def apply(args: List[Value]): Value ={
    var tempEnv = new Environment(defEnv)
    if (parameters.length != args.length) throw new TypeException("# arguments != #parameters")
    else {
      tempEnv.bulkPut(parameters, args)
      body.execute(tempEnv)
    }
  }
}
