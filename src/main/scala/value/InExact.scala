package value

import context.{IllegalValueException, TypeException}

case class InExact(value: Double) extends Numeric with Ordered[Value]{
  def +(other: Value): Addable =
    other match {
      case x: Exact => InExact(this.value + x.value.toDouble)
      case x: InExact => InExact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def *(other:Value):Numeric =
    other match {
      case x: Exact => InExact(this.value * x.value.toDouble)
      case x: InExact => InExact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def -(other:Value):Numeric =
    other match {
      case x: Exact => InExact(this.value - x.value.toDouble)
      case x: InExact => InExact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def /(other:Value):Numeric =
    other match {
      case Exact(0) => throw new IllegalValueException("Cannot divide by 0")
      case x: Exact => InExact(this.value / x.value.toDouble)
      case x: InExact => InExact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = InExact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value.toDouble)
      case x: InExact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }


  override def equals(other: Any): Boolean =
    other match {
      case x: InExact => x.isInstanceOf[InExact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toDouble == this.value
      case _ => false
    }


  override def toString = this.value.toString

  override def hashCode(): Int = super.hashCode()
}
