package expressive

//case class Expression(expr: String) {
//  def evaluate: Int = {
//    ReversePolishNotation.evaluate(Input.parse(expr))
//  }
//}

case class Expression(infixTokens: List[Token]) {
  def evaluate: Int = {
    val rpn = ReversePolishNotation.get(infixTokens)
    ReversePolishNotation.evaluate(rpn)
  }
}

case class Assignment(assignment: String) {

  private val tokens = Input.parse(assignment)
  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  println(s"Assignment tokens: $tokens")
  println(s"Assignment: $leftHand = $rightHand")

  def value: Int = {
    Expression(rightHand).evaluate
  }
}