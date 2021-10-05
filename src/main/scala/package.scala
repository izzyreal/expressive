package object expressive {
  val ops1: Seq[Char] = Seq('+', '-', '/', '*')
  val ops2: Seq[String] = ops1.map(_.toString)

  object implicits {
    implicit class TokenList(tokens: List[Token]) {
      def fancyString: String = {
        tokens.map {
          case Open => "("
          case Close => ")"
          case Plus => "+"
          case Minus => "-"
          case Multiply => "*"
          case Divide => "/"
          case n: Number => n.value.toString
          case i: Identifier => i.name
          case Equals => "="
          case _ => ""
        }.mkString(" ")
      }
    }
  }
}
