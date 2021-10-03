package expressive

class Input(val expression: String) {

  private var pos = -1

  def hasNext: Boolean = pos < expression.length

  // Advances the position by the number of digits found
  private def collectContiguousDigits: String = {
    val digits = expression.substring(pos).takeWhile { _ =>
      val continue = pos < expression.length &&
        expression(pos).isDigit
      pos += 1
      continue
    }
    pos -= 2
    digits
  }

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

      if (c.isDigit) {
        val digits = collectContiguousDigits
        Number(Integer.parseInt(digits))
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
