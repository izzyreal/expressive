package expressive.expression

import expressive.Main
import expressive.implicits.TokenList
import expressive.parser._

case class Function(tokens: List[Token]) {

  private val leftHand = tokens.takeWhile(_ != Equals)
  private val rightHand = tokens.dropWhile(_ != Equals).drop(1)

  def isValid: Boolean =
    leftHand.nonEmpty && rightHand.nonEmpty && name.nonEmpty

  def name: String =
    leftHand.head.asInstanceOf[Identifier].name

  def parameters: List[Identifier] =
    leftHand.dropWhile(_.isInstanceOf[Identifier])
      .dropWhile(_ == Open)
      .takeWhile(_.isInstanceOf[Identifier]).collect { case i: Identifier => i }

  def expand(args: List[Evaluable]): List[Either[String, Evaluable]] = {
    if (args.length != parameters.length) {
      List(Left(s"$name accepts ${parameters.length} arguments, but ${args.length} were provided"))
    } else {
      val paramMap = parameters.zip(args).map(a => a._1.name -> a._2).toMap

      rightHand.map {
        case i: Identifier =>
          // An identifier either refers to an argument
          // that is conveniently stored in a map so
          // it can be referred to multiple times...
          val fromMap = paramMap.get(i.name).collect {
            case n: Number => n.negated(shouldNegate = i.negative)
            case e => e
          }
          // ...or it refers to a variable that is declared
          // on the heap.
          fromMap match {
            case Some(argVar) => Right(argVar)
            case None => Main.heapVars(i.name).value.map(Number).map(_.negated(shouldNegate = i.negative))
          }
        case e: Evaluable => Right(e)
      }
    }
  }

  override def toString: String = s"$name(${
    parameters.collect {
      case i: Identifier => i.name
      case n: Number => n.value.toString
    }.mkString("", ", ", "")
  }) = ${rightHand.fancyString}"
}