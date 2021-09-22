package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def  assignment: Parser[Assignment] = identifier ~ ":=" ~ expression ^^ {
    case identifier ~ ":=" ~ expression => new Assignment(identifier, expression)
  }

    // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
    def  iteration: Parser[Iteration] = "while" ~> "(" ~> expression ~ ")" ~ expression ^^ {
      case  condition ~ ")" ~ body => new Iteration(condition, body)
    }

  // dereference ::= "[" ~ expression ~ "]"
  def  dereference: Parser[Expression] = "["~expression~"]" ^^ {
    case   "["~vbl~"]" => FunCall(Identifier("dereference"), List(vbl))
  }


  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | freeze  | funCall | block |  assignment | dereference | literal | "("~>expression<~")"

}