package context

case class JediException(gripe: String = "Jedi error ") extends Exception(gripe)
