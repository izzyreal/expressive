package expressive.expression

import expressive.parser.{Close, Evaluable, Minus, Multiply, Number, Open, Operator, Plus}

import scala.collection.mutable

// Shunting-yard algorithm
// https://en.wikipedia.org/wiki/Shunting-yard_algorithm
object ReversePolishNotation {

  def get(tokens: List[Evaluable]): List[Evaluable] = {

    val operators = mutable.Stack[Operator]()
    val output = mutable.Queue[Evaluable]()

    tokens.foreach {
      case n: Number => output += n
      case o: Operator if o != Open =>
        if (operators.nonEmpty && operators.top != Open) {
          if (o.precedence <= operators.top.precedence) {
            output += operators.pop().asInstanceOf[Evaluable]
          }
        }
        operators.push(o)
      case Open => operators.push(Open)
      case Close =>
        while (operators.nonEmpty && operators.top != Open)
          output += operators.pop().asInstanceOf[Evaluable]
        if (operators.nonEmpty && operators.top == Open)
          operators.pop()
    }

    while (operators.nonEmpty)
      output += operators.pop().asInstanceOf[Evaluable]

    output.toList
  }

  def evaluate(tokens: List[Evaluable]): Either[String, Double] = {

    val stack = mutable.Stack[Double]()
    var emptyStackError: Either[String, Double] = Right(0)

    tokens.foreach {
      case n: Number => stack.push(n.value)
      case o: Operator =>
        if (stack.size < 2) {
          emptyStackError = Left(s"Insufficient operands provided for $o operator")
        } else {
          val b = stack.pop()
          val a = stack.pop()

          val res = o match {
            case Plus => a + b
            case Minus => a - b
            case Multiply => a * b
            case _ => a / b
          }
          stack.push(res)
        }
    }

    if (emptyStackError.isLeft) {
      emptyStackError
    } else {
      Right(stack.pop())
    }
  }
}
