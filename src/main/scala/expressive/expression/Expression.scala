package expressive.expression

import expressive.Main
import expressive.implicits.TokenList
import expressive.parser.{Close, Evaluable, Identifier, Multiply, Number, Open, Token}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Expression(tokens: List[Token]) {
  private lazy val resolved = Expression.resolveReferences(tokens)

  def evaluate: Either[String, Double] = {
    resolved match {
      case Left(e) => Left(e)
      case Right(r) =>
        val rpn = ReversePolishNotation.get(r)
        ReversePolishNotation.evaluate(rpn)
    }
  }

  override def toString: String = resolved.map(_.fancyString).getOrElse(s"Invalid expression: ${tokens.fancyString}")
}

case object Expression {

  private def resolveVariable(i: Identifier): Either[String, Number] =
    Main.heapVars.get(i.name) match {
      case Some(heapVar) => heapVar.value.map(Number)
      case None => Left(s"Variable ${i.name} has not been declared")
    }

  private def expandFunctionCall(i: Identifier, args: List[Evaluable]): List[Either[String, Evaluable]] = {
    Main.heapFuncs.get(i.name) match {
      case Some(heapFunc) => heapFunc.expand(args)
      case None => List(Left(s"Function ${i.name} has not been declared"))
    }
  }

  def resolveReferences(tokens: List[Token]): Either[String, List[Evaluable]] = {
    val queue = new mutable.Queue[Token]
    tokens.foreach(t => queue.enqueue(t))

    val buf = new ListBuffer[Evaluable]

    def collectReference(i: Identifier, nested: Boolean = false): Either[String, Evaluable] = {
      queue.headOption match {

        // i is the identifier of a function
        case Some(Open) =>
          val args = new ListBuffer[Evaluable]
          var argError: Either[String, Evaluable] = Right(Number(0))

          while (queue.nonEmpty && queue.head != Close && argError.isRight) {
            queue.dequeue() match {
              case i2: Identifier =>
                collectReference(i2, nested = true) match {
                  case Left(e) => argError = Left(e)
                  case Right(a) => args += a
                }
              case n: Number => args += n
              case e: Evaluable => buf += e
            }
          }

          argError match {
            case Left(e) => Left(e)
            case Right(_) =>
              if (nested) queue.dequeue()
              val expandedFunctionCall = expandFunctionCall(i, args.toList)

              if (expandedFunctionCall.collectFirst { case Left(e) => Left(e) }.nonEmpty) {
                Left(s"Error(s) while expanding function call: " +
                  s"${expandedFunctionCall.collect { case Left(e) => e }.mkString("", ", ", "")}")
              } else {

                val prependNegative = if (i.negative) {
                  List(Open)
                } else List.empty

                val appendNegative = if (i.negative) {
                  List(Close, Multiply, Number(-1))
                } else List.empty

                buf.addAll(prependNegative ++ expandedFunctionCall.collect {
                  case Right(evaluable) => evaluable
                } ++ appendNegative)
                Right(Close)
              }
          }

        // i is the identifier of a variable
        case _ => resolveVariable(i).map(_.negated(shouldNegate = i.negative))
      }
    }

    var error: Either[String, Double] = Right(0)

    while (queue.nonEmpty && error.isRight) {
      val t = queue.dequeue()
      t match {
        case e: Evaluable => buf += e
        case i: Identifier =>
          collectReference(i) match {
            case Left(e) => error = Left(e)
            case Right(res) => if (res != Close) buf += res
          }
      }
    }

    error match {
      case Left(e) => Left(e)
      case Right(_) => Right(buf.toList)
    }
  }

}