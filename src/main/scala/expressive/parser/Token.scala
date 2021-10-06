package expressive.parser

sealed trait Token

sealed trait Evaluable extends Token

sealed trait Operator {
  val precedence: Int
}

case object Open extends Evaluable with Operator {
  override val precedence: Int = 0
}

case object Close extends Evaluable

case class Number(value: Double) extends Evaluable

case object Divide extends Evaluable with Operator {
  override val precedence: Int = 3
}

case object Multiply extends Evaluable with Operator {
  override val precedence: Int = 3
}

case object Plus extends Evaluable with Operator {
  override val precedence: Int = 2
}

case object Minus extends Evaluable with Operator {
  override val precedence: Int = 2
}

case class Identifier(name: String, negative: Boolean) extends Token

case object Equals extends Token

case object End extends Token

case object Unknown extends Token
