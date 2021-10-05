package expressive.expression

import expressive.Main
import expressive.implicits.TokenList
import expressive.parser.{Close, Evaluable, Identifier, Number, Open, Token}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Expression(tokens: List[Token]) {
  private lazy val resolved = Expression.resolveReferences(tokens)

  def evaluate: Int = {
    val rpn = ReversePolishNotation.get(resolved)
    ReversePolishNotation.evaluate(rpn)
  }

  override def toString: String = resolved.fancyString
}

case object Expression {

  private def resolveVariable(i: Identifier): Evaluable =
    Number(Main.heapVars(i.name).value)

  private def expandFunctionCall(i: Identifier, args: List[Evaluable]): List[Evaluable] = {
    val f = Main.heapFuncs(i.name)
    val result = f.expand(args)
    println(s"expand result: ${result.fancyString}")
    result
  }

  def resolveReferences(tokens: List[Token]): List[Evaluable] = {
    val queue = new mutable.Queue[Token]
    tokens.foreach(t => queue.enqueue(t))

    val buf = new ListBuffer[Evaluable]

    while (queue.nonEmpty) {
      val t = queue.dequeue()
      t match {
        case e: Evaluable => buf += e
        case i: Identifier =>
          queue.headOption match {

            // i is an identifier of a function
            case Some(Open) =>
              buf += Open
              val args = new ListBuffer[Evaluable]

              while (queue.nonEmpty && queue.head != Close) {
                queue.dequeue() match {
                  case i: Identifier => args += resolveVariable(i)
                  case n: Number => args += n
                  case _ => ()
                }
              }

              buf.addAll(expandFunctionCall(i, args.toList))

            // i is an identifier of a variable
            case _ => buf += resolveVariable(i)
          }
      }
    }

    buf.toList
  }

}