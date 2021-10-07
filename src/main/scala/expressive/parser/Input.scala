package expressive.parser

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
    else expression.substring(pos + 1).takeWhile(c => c.isDigit || c == '.')

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

      var end = false

      while ((c == ' ' || c == ',') && !end) {
        if (pos + 1 >= expression.length) {
          end = true
        } else {
          pos += 1
          c = expression(pos)
        }
      }

      if (end) {
        End
      } else {
        if (c == '=') {
          Equals
        } else if (c.isLetter) {
          val adjacent = adjacentLetters
          pos += adjacent.length
          Identifier(adjacent.prepended(c), negative = false)
        } else if (c.isDigit) {
          val adjacent = adjacentDigits
          pos += adjacent.length
          Number(adjacent.prepended(c).toDouble)
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
          if (pos + 1 < expression.length) {
            val c2 = expression(pos + 1)
            if (c2.isDigit) {
              pos += 1
              val adjacent = adjacentDigits
              pos += adjacent.length
              Number(-1 * adjacent.prepended(c2).toDouble)
            } else if (c2.isLetter) {
              pos += 1
              val adjacent = adjacentLetters
              pos += adjacent.length
              Identifier(adjacent.prepended(c2), negative = true)
            } else Minus
          } else Minus
        } else {
          Unknown
        }
      }
    }
  }
}
