package expressive.expression

import expressive.Main
import expressive.implicits.TokenList
import expressive.parser._

case class Function(declaration: String) {

  private val tokens = Input.parse(declaration)
  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def name: String =
    leftHand.head.asInstanceOf[Identifier].name

  def parameters: List[Identifier] =
    leftHand.dropWhile(_.isInstanceOf[Identifier])
      .dropWhile(_ == Open)
      .takeWhile(_.isInstanceOf[Identifier]).collect { case i: Identifier => i }

  def expand(args: List[Evaluable]): List[Evaluable] = {
    val paramMap = parameters.zip(args).map(a => a._1.name -> a._2).toMap

    rightHand.map {
      case i: Identifier =>
        paramMap.getOrElse(i.name,
          Number(Main.heapVars(i.name).value)
        )
      case e: Evaluable => e
    }
  }

  override def toString: String = s"$name(${
    parameters.collect {
      case i: Identifier => i.name
      case n: Number => n.value.toString
    }.mkString("", ", ", "")
  }) = ${rightHand.fancyString}"
}