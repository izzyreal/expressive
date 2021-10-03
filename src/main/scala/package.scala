package object expressive {
  val ops1: Seq[Char] = Seq('+', '-', '/', '*')
  val ops2: Seq[String] = ops1.map(_.toString)
}
