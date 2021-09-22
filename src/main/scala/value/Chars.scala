package value

import context.TypeException
import expression.Literal

case class Chars(value: String) extends Addable with Ordered[Value] {
  def size(): Exact = Exact(value.length)

  def subChars(to: Exact, from: Exact): Chars = Chars(value.substring(to.value, from.value))

  def +(other:Value) =
    other match{
      case x:Exact => Chars(this.value + x.value.toString)
      case x:InExact => Chars(this.value + x.value.toString)
      case x:Chars => Chars(this.value + x.value)
      case _ => throw new TypeException("Addable operand required")
    }

  override def compare(other: Value): Int =
    other match {
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def hashCode(): Int = super.hashCode()

  override def equals(obj: Any): Boolean =
    obj match {
      case x:Chars =>x.isInstanceOf[Chars] && x.value.equals(this.value)
    }

  override def toString: String = value
}