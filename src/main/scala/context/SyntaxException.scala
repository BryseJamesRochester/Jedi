package context

import scala.util._
import scala.util.parsing.combinator.Parsers

class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")