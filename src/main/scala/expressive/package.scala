import expressive.parser.{Close, Divide, Equals, Identifier, Minus, Multiply, Number, Open, Plus, Token, Unknown}

package object expressive {

  object implicits {

    implicit class TokenList(tokens: List[Token]) {

      def fancyString: String = {
        var previous: Token = Unknown
        var endResult = ""

        tokens.indices foreach { i =>
          val tokenString = tokens(i) match {
            case Open => "("
            case Close => ")"
            case Plus => " + "
            case Minus => " - "
            case Multiply => " * "
            case Divide => " / "
            case Equals => " = "

            case n: Number =>
              val numberString = n.value.toString
              if (previous.isInstanceOf[Number] || previous.isInstanceOf[Identifier]) {
                s", $numberString"
              } else {
                numberString
              }

            case i: Identifier =>
              val res2 = (if (i.negative) "-" else "") + i.name
              if (previous.isInstanceOf[Number] || previous.isInstanceOf[Identifier]) s", $res2" else res2

            case _ => ""
          }

          endResult += tokenString
          previous = tokens(i)
        }

        endResult
      }

    }
  }
}
