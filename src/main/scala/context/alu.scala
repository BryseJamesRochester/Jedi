package context

import value._
import expression._

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "nada" => None.asInstanceOf[Value]
    case "write" => write(args)
    case "dereference" => dereference(args)
    case "var" => makeVar(args)
    case "nil" => getEmpty(args)
    case "cons" => createPair(args)
    case "car" => first(args)
    case "cdr" => second(args)
    case "list" => createList(args)
    case _ => throw new UndefinedException(opcode)

    //case "not" => not(args)            // unary
    // TBC
  }
  }

  private def add(args: List[Value]): Value = {

    def helper(result: Addable, unseen: List[Value]):Addable =
      if (unseen == Nil) result
      else helper(result+unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def sub(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]):Numeric =
      if (unseen == Nil) result
      else helper(result-unseen.head, unseen.tail)

    if (args.size<2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _=> throw new TypeException("Inputs to - must be Numeric")
    }
  }

  private def mul(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]):Numeric = {
      if (unseen == Nil) result
      else helper(result*unseen.head, unseen.tail)
    }

    if (args.size<2) throw new TypeException("2 or more inputs required by *")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _=> throw new TypeException("Inputs to * must be Numeric")
    }
  }

  private def div(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]):Numeric =
      if (unseen == Nil) result
      else helper(result/unseen.head, unseen.tail)

    if (args.size<2) throw new TypeException("2 or more inputs required by /")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _=> throw new TypeException("Inputs to / must be Numeric")
    }
  }

  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  private def same(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    Boole(args(0) == args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    Boole(args(0) != args(1))
  }

  //private def not(args: List[Value]): Value = {
  //  if(args.size != 1) throw new TypeException("1 inputs required by !")
  //  if(!args(0).isInstanceOf[Boole]) throw new TypeException("")
  //  Boole(!args(0).asInstanceOf[Ordered[Value]])
  //}

  private def write(args: List[Value]): Value = {println(args(0));Notification.DONE()}

  // returns the content of args(0)
  private def dereference(args: List[Value]) = {args(0).asInstanceOf[Variable].content}

  // creates a new variable containing args(0)
  private def makeVar(args: List[Value]) = {Variable(args(0))}
  // etc.

  private def getEmpty(args: List[Value]) = if (args.length==0) empty else throw new IllegalValueException("Not Empty")

  private def createPair(args: List[Value]) = {
    args.length match{
      case 1 => new Pair(args(0), empty)
      case 2 => new Pair(args(0), args(1))
      case _ => throw new IllegalValueException("Pairs require 1 or 2 arguments")
    }
  }

  private def first(args: List[Value]) = {args(0).asInstanceOf[Pair].getFirst()}

  private def second(args: List[Value]) = {args(0).asInstanceOf[Pair].getSecond()}

  private def createList(args: List[Value]) = {
    val inList = args.reverse
    var outList = createPair(List(inList.head))
    for (i <- inList.drop(1)){
      outList = createPair(List(i, outList))
    }
    outList
  }
}