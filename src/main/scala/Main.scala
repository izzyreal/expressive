package expressive

import scala.collection.mutable

object Main extends App {

  val variables = new mutable.HashMap[String, Variable]

//  val functions = new ListBuffer[Function]

  val line1 = "x = 2"
  val line2 = "y = x * 3"
  val line3 = "foo(a) = a * x * y"
  val line4 = "x * (y + foo(3)) / 2" // 42

  val variableDeclarations = Seq(
    line1,
    line2,
//    line3
  )

  variableDeclarations.foreach { e =>
    val a = Variable(e)
    variables(a.name) = a
    println(s"${a.value}")
  }

}
