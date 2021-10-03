package expressive

object Parser {
  def apply(n: Node): Unit = {
    val expression = n.str.substring(1, n.str.length - 1)

    var openers = 0
    var closers = 0
    var parsingDigits = false
    val nodeStr = new StringBuilder

    def closure: Boolean = openers == closers

    def appendNode(): Unit = {
      val node = new Node
      node.str = nodeStr.mkString
      n.children.addOne(node)
      nodeStr.clear()
    }

    expression.foreach { c =>

      if (parsingDigits && !c.isDigit) {
        appendNode()
        parsingDigits = false
      }

      if (c != ' ')
        nodeStr += c

      if (c.isDigit && closure) {
        parsingDigits = true
      } else if (ops1.contains(c) && closure) {
        appendNode()
      } else if (c == '(') {
        openers += 1
      } else if (c == ')') {
        closers += 1
        if (closure) {
          appendNode()
          n.children.last.parse()
        }
      }
    }

    if (parsingDigits) {
      appendNode()
    }
  }
}
