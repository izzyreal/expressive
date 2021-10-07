package expressive

import expressive.Memory.{heapFuncs, heapVars}
import expressive.model.{Expression, Variable}
import expressive.implicits.TokenList
import expressive.parser._

import scala.collection.mutable

object Memory {
  val heapVars = new mutable.HashMap[String, Variable]
  val heapFuncs = new mutable.HashMap[String, model.Function]
}

object Main extends App {

  println(s"Welcome to Expressive 1.0")
  println("Type in expressions for evaluation.\n")

  var in = ""

  do {
    print("expressive> ")
    in = Console.in.readLine()

    val tokens = Input.parse(in)

    if (tokens.count(_ == Open) != tokens.count(_ == Close)) {
      println(" Non-matching parentheses found")
    } else if (in != "exit") {

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

  println(" Bye!")

  private def isFunctionDeclaration(tokens: List[Token]): Boolean = {
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

    startsWithIdentifier && opensAfterIdentifier && closesAfterArguments && hasEquals
  }

  def declareVar(tokens: List[Token]): Unit = {
    if (tokens.length < 3) {
      println(s" Invalid variable declaration: ${tokens.fancyString}")
    } else {
      val a = Variable(tokens)
      heapVars(a.name) = a
      println(s" Variable defined: $a")
    }
  }

  def declareFun(tokens: List[Token]): Unit = {
    val f = model.Function(tokens)
    if (!f.isValid) {
      println(s" Invalid function declaration: ${tokens.fancyString}")
    } else {
      heapFuncs(f.name) = f
      println(s" Function defined: $f")
    }
  }

  def evaluateExpression(tokens: List[Token]): Unit = {
    val expr = Expression(tokens)

    expr.evaluate match {
      case Left(e) => println(s" Could not evaluate ${tokens.fancyString}: $e")
      case Right(result) => println(s" $expr = $result")
    }
  }
}
