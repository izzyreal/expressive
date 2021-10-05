package expressive

import expressive.expression.{Expression, Variable}
import expressive.implicits.TokenList
import expressive.parser.{Input, Number}

import scala.collection.mutable

object Main extends App {

  val heapVars = new mutable.HashMap[String, Variable]
  val heapFuncs = new mutable.HashMap[String, expression.Function]
  val stackVals = new mutable.HashMap[String, Int]

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

  val foo = expression.Function(line3)
  //  println(s"$foo = ${foo.value(List(Number(1)))}")
  heapFuncs(foo.name) = foo
  println(s"$foo")

  val tokens = Input.parse(line4)
  println(s"${tokens.fancyString}")
  val expr = Expression(tokens)
  println(s"${expr} = ${expr.evaluate}")

}
