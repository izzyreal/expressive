package expressive

import scala.collection.mutable

// Shunting-yard algorithm
// https://en.wikipedia.org/wiki/Shunting-yard_algorithm
object ReversePolishNotation {

  def get(tokens: List[Token]): List[Token] = {

    val operators = mutable.Stack[Operator]()
    val output = mutable.Queue[Token]()

    tokens.foreach {
      case Unknown | End => ()
      case n: Number => output += n
      case o: Operator if o != Open => operators.push(o)
      case o: Operator =>
        var stop = false
        while (operators.nonEmpty && !stop) {
          val op2 = operators.top
          if (o.precedence > op2.precedence) {
            output += operators.pop().asInstanceOf[Token]
          } else {
            stop = true
          }
        }
        operators += o

      case Open => operators += Open
      case Close =>
        while (operators.nonEmpty && operators.top != Open)
          output += operators.pop().asInstanceOf[Token]
        if (operators.nonEmpty && operators.top == Open)
          operators.pop()
        if (operators.nonEmpty && operators.top != Open)
          output += operators.pop().asInstanceOf[Token]
    }

    while (operators.nonEmpty)
      output += operators.pop().asInstanceOf[Token]

    output.toList
  }
}
