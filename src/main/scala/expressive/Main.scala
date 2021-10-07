package expressive

import expressive.expression.{Expression, Variable}
import expressive.parser._

import scala.collection.mutable

object Main extends App {

  val heapVars = new mutable.HashMap[String, Variable]
  val heapFuncs = new mutable.HashMap[String, expression.Function]

//  test()

  println("Welcome to Expressive 1.0")
  println("Type in expressions for evaluation.\n")

  var in = ""
  do {
    print("expressive> ")
    in = Console.in.readLine()

    val tokens = Input.parse(in)

    val t1 = tokens.headOption
    val t2 = tokens.drop(1).headOption

    (t1, t2) match {
      case (Some(_: Identifier), Some(Open)) => declareFun(in)
      case (Some(_: Identifier), Some(Equals)) => declareVar(tokens)
      case (None, _) => println("\nInvalid command.\n")
      case _ => evaluateExpression(in)
    }

  } while (in != "exit")

  def declareVar(tokens: List[Token]): Unit = {
    val a = Variable(tokens)
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
    //    println(s"Input tokens: ${tokens.fancyString}")
    val expr = Expression(tokens)
    println(s"$expr = ${expr.evaluate}")
  }

  def test(): Unit = {
    declareVar(Input.parse("x = 2"))
    declareVar(Input.parse("y = x * 3"))
    declareFun("foo(a) = a * x * y")
    evaluateExpression("x * (y + foo(3)) / 2") // 42.0
    declareFun("bar(a) = a * -4")
    evaluateExpression("x * (y + bar(foo(bar(foo(3))))) / 2") // 6918.0
    declareVar(Input.parse("x  = 1 + 2.5 * 4"))
    declareVar(Input.parse("y = x * 2 + x"))
    evaluateExpression("x + y - 2")
    declareFun("foo(a, b) = (a + b) / b")
    declareFun("bar(c) = -c")
    evaluateExpression("foo(1, 2) + bar(3.5) + 1")
  }
}
