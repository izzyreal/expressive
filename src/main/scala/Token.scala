package expressive

sealed trait Token

sealed trait Operator {
  val precedence: Int
}

case object Open extends Token with Operator {
  override val precedence: Int = 0
}

case object Close extends Token

case class Number(value: Int) extends Token

case class Identifier(name: String) extends Token

case object Equals extends Token

//trait HasName {
//  val name: String
//}

//case class Variable(name: String, expr: List[Token]) extends Token with HasName {
//
//}
//
//case class Function(name: String, expr: List[Token]) extends Token with HasName {
//
//}

case object Divide extends Token with Operator {
  override val precedence: Int = 3
}

case object Multiply extends Token with Operator {
  override val precedence: Int = 3
}

case object Plus extends Token with Operator {
  override val precedence: Int = 2
}

case object Minus extends Token with Operator {
  override val precedence: Int = 2
}

case object End extends Token

case object Unknown extends Token
