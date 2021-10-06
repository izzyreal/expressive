package expressive.expression

import expressive.implicits.TokenList
import expressive.parser.{Equals, Identifier, Input}

case class Variable(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals).head
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.asInstanceOf[Identifier].name

  def value: Double = {
    Expression(rightHand).evaluate
  }

  override def toString: String = s"$name = ${rightHand.fancyString}"
}
