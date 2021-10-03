package expressive

object Evaluator {
  def apply(n: Node): Int = {
    if (n.isValue) {
      Integer.parseInt(n.str)
    } else {

      var op = ""

      n.children.foldLeft(0) { (acc, cur) =>
        if (cur.children.nonEmpty) {
          cur.value()
        } else if (cur.isValue && op == "") {
          cur.value()
        } else if (op == "+") {
          op = ""
          acc + cur.value()
        } else if (op == "-") {
          op = ""
          acc - cur.value()
        } else if (op == "/") {
          op = ""
          acc / cur.value()
        } else if (op == "*") {
          op = ""
          acc * cur.value()
        } else {
          op = cur.str
          acc
        }
      }
    }
  }
}
