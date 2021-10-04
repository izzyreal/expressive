package expressive

//case class Expression(expr: String) {
//  def evaluate: Int = {
//    ReversePolishNotation.evaluate(Input.parse(expr))
//  }
//}

case class Expression(tokens: List[Token]) {

  private def resolved: List[Token] = {
    tokens.map {
      case i: Identifier => Number(Main.variables(i.name).value)
      case other => other
    }
  }

  def evaluate: Int = {
    val rpn = ReversePolishNotation.get(resolved)
    ReversePolishNotation.evaluate(rpn)
  }
}

case class Variable(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals).head
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  println(s"Var: $name = $rightHand")

  def name: String =
    leftHand.asInstanceOf[Identifier].name

  def value: Int = {
    Expression(rightHand).evaluate
  }
}

case class Function(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  println(s"Var: $name = $rightHand")

  def name: String =
    leftHand.head.asInstanceOf[Identifier].name

  def value(inputs: List[Token]): Int = {
    Expression(rightHand).evaluate
  }
}