package expressive

import scala.collection.mutable.ListBuffer

case object Input {
  def parse(str: String): List[Token] = {
    val in = new Input(str)
    val tokens = new ListBuffer[Token]
    while (in.hasNext) {
      val next = in.next
      if (next != End) {
        tokens += next
      }
    }
    tokens.toList
  }
}

class Input(val expression: String) {

  private var pos = -1

  private def hasNext: Boolean = pos < expression.length

  private def adjacentDigits: String =
    if (pos + 1 >= expression.length) ""
    else expression.substring(pos + 1).takeWhile(_.isDigit)

  private def adjacentLetters: String =
    if (pos + 1 >= expression.length) ""
    else expression.substring(pos + 1).takeWhile(_.isLetter)

  private def reachedEnd: Boolean =
    pos >= expression.length

  def next: Token = {

    pos += 1

    if (reachedEnd) {
      End
    } else {
      var c = expression(pos)

      while (c == ' ') {
        pos += 1
        c = expression(pos)
      }

      if (c == '=') {
        Equals
      } else if (c.isLetter) {
        val adjacent = adjacentLetters
        pos += adjacent.length
        Identifier(adjacent.prepended(c))
      } else if (c.isDigit) {
        val adjacent = adjacentDigits
        pos += adjacent.length
        Number(Integer.parseInt(adjacent.prepended(c)))
      } else if (c == '(') {
        Open
      } else if (c == ')') {
        Close
      } else if (c == '/') {
        Divide
      } else if (c == '*') {
        Multiply
      } else if (c == '+') {
        Plus
      } else if (c == '-') {
        Minus
      } else {
        Unknown
      }
    }
  }
}
