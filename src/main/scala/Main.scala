package expressive

object Main extends App {

  val expr1 = "(1 + (1-4) + 11)"
  val expr2 = "( (10 + 5) / 3)"
  val expr3 = "(1 - 1 + 1)"
  val expr4 = "(1+2)*(3+(4*5))"
  val expr5 = "(1+(4*5))"

  val root = new Node
  root.str = expr5

  root.parse()

  println(root.value())
}
