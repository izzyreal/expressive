package expressive

import scala.collection.mutable

// Shunting-yard algorithm
// https://en.wikipedia.org/wiki/Shunting-yard_algorithm
object ReversePolishNotation {

  def get(tokens: List[Token]): List[Token] = {

    val operators = mutable.Stack[Token]()
    val output = mutable.Queue[Token]()

    tokens.foreach {
      case Unknown | End => ()
      case n: Number => output += n
      case o: Operator =>
        while (operators.nonEmpty &&
          (operators.top.isInstanceOf[Operator] &&
          operators.top.asInstanceOf[Operator].precedence >= o.precedence)) {

          output += operators.pop()
        }

        operators.push(o)

      case Open => operators += Open
      case Close =>
        var _break = false
//        while (operators.nonEmpty && !_break) {
//          val popped = operators.pop()
//          if (popped == Open) _break = true
//          else output += popped
//        }
        while (operators.nonEmpty && operators.top != Open)
          output += operators.pop()
        if (operators.nonEmpty && operators.top == Open)
          operators.pop()
    }

    while (operators.nonEmpty)
      output += operators.pop()

    output.toList
  }
}
