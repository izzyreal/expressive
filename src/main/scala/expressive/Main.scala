package expressive

import expressive.expression.{Expression, Variable}
import expressive.implicits.TokenList
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

    if (tokens.count(_ == Open) != tokens.count(_ == Close)) {
      println("Non-matching parentheses found")
    } else {

      val t1 = tokens.headOption
      val t2 = tokens.drop(1).headOption

      (t1, t2) match {
        case (Some(_: Identifier), Some(Open)) =>
          if (isFunctionDeclaration(tokens))
            declareFun(tokens)
          else evaluateExpression(tokens)
        case (Some(_: Identifier), Some(Equals)) => declareVar(tokens)
        case (None, _) => ()
        case _ => evaluateExpression(tokens)
      }
    }

  } while (in != "exit")

  def isFunctionDeclaration(tokens: List[Token]): Boolean = {
    val startsWithIdentifier = tokens.headOption match {
      case Some(_: Identifier) => true
      case _ => false
    }

    val opensAfterIdentifier = tokens.drop(1).headOption match {
      case Some(Open) => true
      case _ => false
    }

    val closesAfterArguments = tokens.contains(Close)
    val hasEquals = tokens.contains(Equals)
    val hasBody = tokens.dropWhile(_ != Equals).nonEmpty

    startsWithIdentifier && opensAfterIdentifier && closesAfterArguments && hasEquals && hasBody
  }

  def declareVar(tokens: List[Token]): Unit = {
    if (tokens.length < 3) {
      println(s"Invalid variable declaration: ${tokens.fancyString}")
    } else {
      val a = Variable(tokens)
      heapVars(a.name) = a
      println(s"Variable declared: $a")
    }
  }

  def declareFun(tokens: List[Token]): Unit = {
    val f = expression.Function(tokens)
    if (!f.isValid) {
      println(s"Invalid function declaration: ${tokens.fancyString}")
    } else {
      heapFuncs(f.name) = f
      println(s"Function declared: $f")
    }
  }

  def evaluateExpression(tokens: List[Token]): Unit = {
    val expr = Expression(tokens)

    expr.evaluate match {
      case Left(e) => println(s"Could not evaluate ${tokens.fancyString}: $e")
      case Right(result) => println(s"$expr = $result")
    }
  }

  def test(): Unit = {
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
}
