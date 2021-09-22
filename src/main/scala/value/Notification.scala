package value

class Notification extends Value{
  var message: String = ""

  override def toString: String = this.message.toString
}

object Notification{
  def apply(message: String): Notification = {
    val n = new Notification
    n.message = message
    n
  }

  def OK() = {
    val n = new Notification
    n.message = "ok"
    n
  }

  def DONE() = {
    val n = new Notification
    n.message = "done"
    n
  }

  def UNSPECIFIED() = {
    val n = new Notification
    n.message = "unspecified"
    n
  }
}
