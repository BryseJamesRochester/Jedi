package value

class Pair(first:Value, second:Value) extends Value{
  override def toString: String = "(" + first + ", " + second + ")"

  def getFirst() = first
  def getSecond() = second
}
