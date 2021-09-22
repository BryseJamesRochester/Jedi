package value

import context.Environment
import expression.Expression

class Thunk(body:Expression, defEnv:Environment, var cache:Value = null) extends Closure(defEnv, body, Nil) {

  override def toString: String = cache.toString

  def apply(): Value ={
    if (cache != null) cache
    else {
      cache = body.execute(defEnv)
      cache
    }
  }
}
