package expressive

import scala.collection.mutable.ArrayBuffer

class Node {
  var str: String = ""
  val children: ArrayBuffer[Node] = ArrayBuffer.empty

  def isValue: Boolean = str.forall(_.isDigit)
  def isOp: Boolean = ops2.contains(str)

  def parse(): Unit = {
    Parser(this)
  }

  def value(): Int = {
    Evaluator(this)
  }
}
