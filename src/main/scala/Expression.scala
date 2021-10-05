package expressive

import implicits.TokenList

case class Expression(tokens: List[Token]) {

  private def resolved: List[Token] = {
    tokens.map {
      case i: Identifier =>
        Number {
          Main.stackVals.getOrElse(i.name,
            Main.heapVars(i.name).value)
        }
      case other => other
    }
  }

  def evaluate: Int = {
    val rpn = ReversePolishNotation.get(resolved)
    ReversePolishNotation.evaluate(rpn)
  }

  override def toString: String = tokens.fancyString
}

case class Variable(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals).head
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.asInstanceOf[Identifier].name

  def value: Int = {
    Expression(rightHand).evaluate
  }

  override def toString: String = s"$name = ${rightHand.fancyString}"
}

case class Function(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.head.asInstanceOf[Identifier].name

  private def loadParameters(parameters: List[Token]): Unit = {
    val values = parameters.map {
      case i: Identifier => Main.heapVars(i.name).value
      case n: Number => n.value
    }

    val inputs = leftHand.collect { case i: Identifier => i }.drop(1)

    if (values.length == inputs.length) {
      inputs.zip(values).foreach(input => Main.stackVals(input._1.name) = input._2)
    }
  }

  def value(parameters: List[Token]): Int = {
    loadParameters(parameters)
    val res = Expression(rightHand).evaluate
    Main.stackVals.clear()
    res
  }

  def arguments: List[Token] =
    leftHand.dropWhile(_.isInstanceOf[Identifier])
      .dropWhile(_ == Open)
      .takeWhile(_.isInstanceOf[Identifier])

  override def toString: String = s"$name(${arguments.fancyString}) = ${rightHand.fancyString}"
}