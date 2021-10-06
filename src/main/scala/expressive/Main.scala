package expressive

import expressive.expression.{Expression, Variable}
import expressive.implicits.TokenList
import expressive.parser.Input

import scala.collection.mutable

object Main extends App {

  val heapVars = new mutable.HashMap[String, Variable]
  val heapFuncs = new mutable.HashMap[String, expression.Function]

  declareVar("x = 2")
  declareVar("y = x * 3")
  declareFun("foo(a) = a * x * y")
  evaluateExpression("x * (y + foo(3)) / 2") // 42.0
  declareFun("bar(a) = a * -4")
  evaluateExpression("x * (y + bar(foo(bar(foo(3))))) / 2") // 6918.0
  declareVar("x  = 1 + 2.5 * 4")
  declareVar("y = x * 3 + x")
  evaluateExpression("x + y - 2")
  declareFun("foo(a, b) = (a + b) / b")
  declareFun("bar(c) = -c")
  evaluateExpression("foo(1, 2) + bar(3.5) + 1")

  def declareVar(line: String): Unit = {
    val a = Variable(line)
    heapVars(a.name) = a
    println(a)
  }

  def declareFun(line: String): Unit = {
    val f = expression.Function(line)
    heapFuncs(f.name) = f
    println(s"$f")
  }

  def evaluateExpression(line: String): Unit = {
    val tokens = Input.parse(line)
    println(s"Input tokens: ${tokens.fancyString}")
    val expr = Expression(tokens)
    println(s"$expr = ${expr.evaluate}")
  }

}
