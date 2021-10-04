package expressive

import scala.collection.mutable

object Main extends App {

  val heapVars = new mutable.HashMap[String, Variable]
  val stackVals = new mutable.HashMap[String, Int]
//  val functions = new ListBuffer[Function]

  val line1 = "x = 2"
  val line2 = "y = x * 3"
  val line3 = "foo(a) = a * x * y"
  val line4 = "x * (y + foo(3)) / 2" // 42

  val variableDeclarations = Seq(
    line1,
    line2
  )

  variableDeclarations.foreach { e =>
    val a = Variable(e)
    heapVars(a.name) = a
//    println(s"${a.value}")
  }

  val foo = Function(line3)
  println(foo.value(List(Number(1))))

}
