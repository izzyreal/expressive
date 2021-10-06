package expressive.expression

import expressive.implicits.TokenList
import expressive.parser.{Equals, Identifier, Token}

case class Variable(val tokens: List[Token]) {

  private val leftHand = tokens.takeWhile(_ != Equals).head
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.asInstanceOf[Identifier].name

  def value: Double = {
    Expression(rightHand).evaluate
  }

  override def toString: String = s"$name = ${rightHand.fancyString}"
}
