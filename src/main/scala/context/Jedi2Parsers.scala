package context

import expression._

class Jedi2Parsers extends JediParsers {

  //freeze ::= "freeze" ~ "(" ~ expression ~ ")"
  def freeze: Parser[MakeThunk] = "freeze" ~> "(" ~> expression <~ ")" ^^ {
    case exp=> new MakeThunk(exp)
  }

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def  params: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep("," ~ identifier)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => Nil
    case "(" ~ Some(id ~ Nil) ~ ")" =>  List(id)
    case "(" ~ Some(id ~ more) ~ ")" => {
      var list:List[Identifier] = List(id)
      for (identifier <- more) identifier match {
        case "," ~ id => list = id::list
      }
      list.reverse
    }
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def  lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ params ~ expression => new Lambda(params, expression)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def  block: Parser[Block] = "{" ~ expression ~ rep(";" ~ expression) ~ "}" ^^ {
    case "{" ~ expression ~ Nil ~ "}" => Block(List(expression))
    case "{" ~ expression ~ more ~ "}" => {
      var list:List[Expression] = List(expression)
      for (exp <- more) exp match {
        case ";" ~ exp => list = exp::list
      }
      Block(list.reverse)
    }
  }

  // override of term parser
  override def term: Parser[Expression]  = lambda | freeze | funCall | block | literal | "("~>expression<~")"
}
