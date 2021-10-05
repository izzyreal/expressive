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

  private def expandFunctionCall(i: Identifier, args: List[Evaluable]): List[Evaluable] =
    Main.heapFuncs(i.name).expand(args)

  def resolveReferences(tokens: List[Token]): List[Evaluable] = {
    val queue = new mutable.Queue[Token]
    tokens.foreach(t => queue.enqueue(t))

    val buf = new ListBuffer[Evaluable]

    def collectReference(i: Identifier, nested: Boolean = false): Evaluable = {
      queue.headOption match {

        // i is the identifier of a function
        case Some(Open) =>
          val args = new ListBuffer[Evaluable]

          while (queue.nonEmpty && queue.head != Close) {
            queue.dequeue() match {
              case i2: Identifier =>
                args += collectReference(i2, nested = true)
              case n: Number => args += n
              case e: Evaluable => buf += e
            }
          }

          if (nested) queue.dequeue()

          buf.addAll(expandFunctionCall(i, args.toList))
          Close

        // i is the identifier of a variable
        case _ => resolveVariable(i)
      }
    }

    while (queue.nonEmpty) {
      val t = queue.dequeue()
      t match {
        case e: Evaluable => buf += e
        case i: Identifier =>
          val res = collectReference(i)
          if (res != Close) buf += res
      }
    }
    buf.toList
  }

}