package expressive

import scala.collection.mutable.ListBuffer

object Main extends App {

  val expr1 = "(1 + (1-4) + 11)"
  val expr2 = "( (10 + 5) / 3)"
  val expr3 = "(10 - (1 + 1))"
  val expr4 = "(1+2)*(3+(4*5))"
  val expr5 = "(1+(4*5))"
  val expr6 = "((1) + (1))"

  val exprs = Seq(
    expr1,
    expr2,
    expr3,
    expr4,
    expr5,
    expr6
  )

  exprs.foreach { e =>
    val in = new Input(e)

    val tokens = new ListBuffer[Token]

//    println("-= Infix Notation =-")

    while (in.hasNext) {
      val next = in.next
      if (next != End) {
        tokens += next
//        print(s"$next, ")
      }
    }

//    println("")
//    println("")

    val rpnTokens = ReversePolishNotation.get(tokens.toList)

//    println("-= Reverse Polish Notation =-")
//    rpnTokens.foreach { t => print(s"$t, ") }
//    println("")
//    println("")

    println(s"$e = ${ReversePolishNotation.evaluate(rpnTokens)}")
  }

}
