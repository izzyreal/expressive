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
      case o: Operator if o != Open => operators.push(o)
      case o: Operator =>
        var stop = false
        while (operators.nonEmpty && !stop) {
          val op2 = operators.top
          if (o.precedence > op2.precedence) {
            output += operators.pop().asInstanceOf[Evaluable]
          } else {
            stop = true
          }
        }
        operators.push(o)

      case Close =>
        while (operators.nonEmpty && operators.top != Open)
          output += operators.pop().asInstanceOf[Evaluable]
        if (operators.nonEmpty && operators.top == Open)
          operators.pop()
        if (operators.nonEmpty && operators.top != Open)
          output += operators.pop().asInstanceOf[Evaluable]
    }

    while (operators.nonEmpty)
      output += operators.pop().asInstanceOf[Evaluable]

    output.toList
  }

  def evaluate(tokens: List[Evaluable]): Double = {

    val stack = mutable.Stack[Double]()

    tokens.foreach {
      case n: Number => stack.push(n.value)
      case o: Operator =>
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

    stack.pop()
  }
}