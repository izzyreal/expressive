import expressive.parser.{Close, Divide, Equals, Identifier, Minus, Multiply, Open, Plus, Token, Number}

package object expressive {
  object implicits {
    implicit class TokenList(tokens: List[Token]) {
      def fancyString: String = {
        tokens.map {
          case Open => "("
          case Close => ")"
          case Plus => " + "
          case Minus => " - "
          case Multiply => " * "
          case Divide => " / "
          case n: Number => n.value.toString
          case i: Identifier => i.name
          case Equals => " = "
          case _ => ""
        }.mkString
      }
    }
  }
}
