package value

import context.{IllegalValueException, TypeException}
import expression._

case class Exact(val value: Int) extends Numeric with Ordered[Value] {

  def +(other: Value): Addable =
    other match {
      case x: Exact => Exact(this.value + x.value)
      case x: InExact => InExact(this.value.toDouble + x.value)

      case _ => throw new TypeException("Numeric operand required")
    }

  def *(other:Value):Numeric =
    other match {
      case x: Exact => Exact(this.value * x.value)
      case x: InExact => InExact(this.value.toDouble * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def -(other:Value):Numeric =
    other match {
      case x: Exact => Exact(this.value - x.value)
      case x: InExact => InExact(this.value.toDouble - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def /(other:Value):Numeric =
    other match {
      case Exact(0) => throw new IllegalValueException("Cannot divide by 0")
      case x: Exact => Exact(this.value / x.value)
      case x: InExact => InExact(this.value.toDouble / x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = Exact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value)
      case x: InExact => this.value.toDouble.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }


  override def equals(other: Any): Boolean =
    other match {
      case x: InExact => x.isInstanceOf[InExact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }


  override def toString = this.value.toString

  override def hashCode(): Int = super.hashCode()



  // hashCode, etc.
}