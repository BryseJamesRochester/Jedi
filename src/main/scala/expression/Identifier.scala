package expression

import context.Environment
import value.{Thunk, Value}

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) ={
    var result:Value = env(this)
    if (result.isInstanceOf[Thunk]) result = result.asInstanceOf[Thunk].apply()
    result
  }
}
