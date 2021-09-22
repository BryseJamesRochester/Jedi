package value

import context.TypeException
import expression.Literal

case class Boole(value:Boolean) extends Literal{
  def &&(other:Value):Boole =
    other match{
      case x:Boole =>Boole(x.value && this.value)
      case _ => throw new TypeException("Boole operand required")
    }
  def ||(other:Value):Boole =
    other match{
      case x:Boole =>Boole(x.value || this.value)
      case _ => throw new TypeException("Boole operand required")
    }
  def unary_!():Boole =Boole(!this.value)

  def True() = Boole(true)
  def False() = Boole(false)

  override def hashCode(): Int = super.hashCode()

  override def equals(obj: Any): Boolean =
    obj match{
      case x:Boole => x.isInstanceOf[Boole] && x.value==this.value
      case _ => false
    }

  override def toString: String = value.toString
}
