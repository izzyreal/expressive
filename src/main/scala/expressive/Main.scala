package expressive

import expressive.expression.{Expression, Variable}
import expressive.implicits.TokenList
import expressive.parser.Input

import scala.collection.mutable

object Main extends App {

  val heapVars = new mutable.HashMap[String, Variable]
  val heapFuncs = new mutable.HashMap[String, expression.Function]
  val stackVals = new mutable.HashMap[String, Int]

  val line1 = "x = 2"
  val line2 = "y = x * 3"
  val line3 = "foo(a) = a * x * y"
  val line4 = "x * (y + foo(3)) / 2" // 42
  val line5 = "bar(a) = a * 4"
  val line6 = "x * (y + bar(foo(bar(foo(3))))) / 2" // 42

  val variableDeclarations = Seq(
    line1,
    line2
  )

  variableDeclarations.foreach { declaration =>
    val a = Variable(declaration)
    heapVars(a.name) = a
    //    println(s"${a.value}")
  }

  val functionDeclarations = Seq(
    line3,
    line5
  )

  functionDeclarations.foreach { declaration =>
    val f = expression.Function(declaration)
    heapFuncs(f.name) = f
    println(s"$f")
  }

  val expressions = Seq(line4, line6)

  expressions.foreach { expression =>
    val tokens = Input.parse(expression)
    println(s"Input tokens: ${tokens.fancyString}")
    val expr = Expression(tokens)
    println(s"${expr} = ${expr.evaluate}")
  }
}
