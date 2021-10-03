package expressive

import Main.Token

import scala.collection.mutable

class Node(val value: NodeValue, val left: Option[Node], val right: Option[Node])

object AST {

  // Based on https://www.klittlepage.com/2013/12/22/twelve-days-2013-shunting-yard-algorithm/

  def eval(tree: Node): Int = {
    val value = tree.value
    value match {
      case _: MultiplyOperator =>
        eval(tree.left.get) * eval(tree.right.get)
      case _: DivideOperator =>
        eval(tree.left.get) / eval(tree.right.get)
      case _: PlusOperator =>
        eval(tree.left.get) + eval(tree.right.get)
      case _: MinusOperator =>
        eval(tree.left.get) - eval(tree.right.get)
      case _: Number => value.asInstanceOf[Number].value
    }
  }

  private def addNode(stack: mutable.Stack[Node], value: NodeValue): Unit = {
    val rightNode = stack.pop()
    val leftNode = stack.pop()
    stack.push(new Node(value, Some(leftNode), Some(rightNode)))
  }

  def from(tokens: List[Token]): Node = {

    val operatorStack = mutable.Stack.empty[Operator]
    val operandStack = mutable.Stack.empty[Node]

    tokens.foreach { t =>
      if (t.isOpener) {
        operatorStack.push(new OpenOperator)
      } else if (t.isCloser) {
        var continueForeach = false

        while (operatorStack.nonEmpty && !continueForeach) {
          val popped = operatorStack.pop()
          if (popped.isInstanceOf[OpenOperator]) {
            continueForeach = true
          } else {
            addNode(operandStack, popped)
          }
        }
      } else {
        if (t.isOp) {
          val op1 = t.toOperator
          var stop = false
          while (operatorStack.nonEmpty && !stop) {
            val op2 = operatorStack.top
            if ((op1.rightAssociative && op1.comparePrecedence(op2) == 0) || op1.comparePrecedence(op2) < 0) {
              operatorStack.pop()
              addNode(operandStack, op2)
            } else {
              stop = true
            }
          }
          operatorStack.push(t.toOperator)
        } else if (t.isValue) {
          operandStack.push(new Node(new Number(t.value), None, None))
        }
      }
    }

    while (operatorStack.nonEmpty) addNode(operandStack, operatorStack.pop())

    operandStack.pop()
  }
}
