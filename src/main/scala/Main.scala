package expressive

import scala.collection.mutable.ListBuffer

object Main extends App {

  val variables = new ListBuffer[Assignment]
//  val functions = new ListBuffer[Function]

  val line1 = "x = 2"
  val line2 = "y = x * 3"
  val line3 = "foo(a) = a * x * y"
  val line4 = "x * (y + foo(3)) / 2" // 42

  val assignments = Seq(
    line1,
    line2,
    line3
  )

  assignments.foreach { e =>
    Assignment(e)
  }

}
