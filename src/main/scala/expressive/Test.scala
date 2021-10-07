package expressive

import expressive.Main.{declareFun, declareVar, evaluateExpression}
import expressive.parser.Input

object Test extends App {
  declareVar(Input.parse("x = 2"))
  declareVar(Input.parse("y = x * 3"))
  declareFun(Input.parse("foo(a) = a * x * y"))
  evaluateExpression(Input.parse("x * (y + foo(3)) / 2")) // 42.0
  declareFun(Input.parse("bar(a) = a * -4"))
  evaluateExpression(Input.parse("x * (y + bar(foo(bar(foo(3))))) / 2")) // 6918.0
  declareVar(Input.parse("x  = 1 + 2.5 * 4"))
  declareVar(Input.parse("y = x * 2 + x"))
  evaluateExpression(Input.parse("x + y - 2"))
  declareFun(Input.parse("foo(a, b) = (a + b) / b"))
  declareFun(Input.parse("bar(c) = -c"))
  evaluateExpression(Input.parse("foo(1, 2) + bar(3.5) + 1"))
}
