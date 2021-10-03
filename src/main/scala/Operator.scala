package expressive

abstract class NodeValue

class Number(val value: Int) extends NodeValue

sealed trait Operator extends NodeValue {
  val precedence: Int
  val rightAssociative: Boolean

  def comparePrecedence(o: Operator): Int =
    if (precedence > o.precedence) 1 else if (o.precedence == precedence) 0 else -1
}

class OpenOperator extends Operator {
  override val precedence: Int = 0
  override val rightAssociative: Boolean = false
}

class PlusOperator extends Operator {
  override val precedence: Int = 2
  override val rightAssociative: Boolean = false
}

class MinusOperator extends Operator {
  override val precedence: Int = 2
  override val rightAssociative: Boolean = false
}

class MultiplyOperator extends Operator {
  override val precedence: Int = 3
  override val rightAssociative: Boolean = false
}

class DivideOperator extends Operator {
  override val precedence: Int = 3
  override val rightAssociative: Boolean = false
}