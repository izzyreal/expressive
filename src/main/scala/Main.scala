package expressive

import scala.collection.mutable.ListBuffer

object Main extends App {

  val expr1 = "(1 + (1-4) + 11)"
  val expr2 = "( (10 + 5) / 3)"
  val expr3 = "(10 - (1 + 1))"
  val expr4 = "(1+2)*(3+(4*5))"
  val expr5 = "(1+(4*5))"
  val expr6 = "((1) + (1))"

  val exprs = Seq(expr1, expr2, expr3, expr4, expr5, expr6)

  case class Token(str: String) {
    def value: Int = Integer.parseInt(str)

    def isValue: Boolean = str.nonEmpty && str.forall(_.isDigit)

    def isOp: Boolean = ops2.contains(str)

    def isOpener: Boolean = str == "("

    def isCloser: Boolean = str == ")"

    def isEndOfFile: Boolean = str.isEmpty

    def validate(next: Token): Boolean = {
      val validValue = isValue && (next.isOp || next.isCloser)
      val validOp = isOp && (next.isValue || next.isOpener)
      val validOpener = isOpener && (next.isValue || next.isOpener)
      val validCloser = isCloser && (next.isOp || next.isCloser || next.isEndOfFile)
      validValue || validOp || validOpener || validCloser
    }

    def toOperator: Operator = {
      if (str == "-") new MinusOperator()
      else if (str == "+") new PlusOperator()
      else if (str == "*") new MultiplyOperator()
      else new DivideOperator()
    }
  }

  def tokenize(expr: String): List[Token] = {
    val result: ListBuffer[Token] = ListBuffer.empty
    var pos = 0
    while (pos < expr.length) {

      val c = expr(pos)

      c match {
        case '(' | ')' | '*' | '/' | '+' | '-' =>
          result += Token(c.toString)
          pos += 1

        case c if c.isDigit =>
          val digits = new StringBuilder

          do {
            digits += expr(pos)
            pos += 1
          }
          while (pos < expr.length && expr(pos).isDigit)

          result += Token(digits.mkString)

        case _ => pos += 1
      }
    }

    result += Token("")

    result.toList
  }

  def validate(tokens: List[Token]): Boolean = {
    var pos = 0
    var valid = tokens.count(_.isOpener) == tokens.count(_.isCloser)
    while (pos < tokens.length - 1 && valid) {
      val t1 = tokens(pos)
      val t2 = tokens(pos + 1)
      valid = t1.validate(t2)
      pos += 1
    }
    valid
  }

  exprs.foreach { e =>
    val tokenized = tokenize(e)
    if (validate(tokenized)) {
      val ast = AST.from(tokenized)
      println(s"$e = ${AST.eval(ast)}")
    }
  }
}
