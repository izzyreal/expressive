package expressive.expression

import expressive.Main
import expressive.implicits.TokenList
import expressive.parser._

import scala.collection.mutable

case class Function(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.head.asInstanceOf[Identifier].name

  def arguments: List[Token] =
    leftHand.dropWhile(_.isInstanceOf[Identifier])
      .dropWhile(_ == Open)
      .takeWhile(_.isInstanceOf[Identifier])

  def expand(args: List[Evaluable]): List[Evaluable] = {
    val argsQueue = new mutable.Queue[Evaluable]
    args.foreach(argsQueue += _)

    rightHand.map {
      case i: Identifier =>
        if (leftHand.contains(i))
          argsQueue.dequeue()
        else Number(Main.heapVars(i.name).value)
      case e: Evaluable => e
    }
  }

  override def toString: String = s"$name(${arguments.fancyString}) = ${rightHand.fancyString}"
}