package hu.lexan.mal

import hu.lexan.mal.ast._
import hu.lexan.mal.error.{MalError, MalEvaluationError, MalParseError}
import hu.lexan.mal.eval.Interpreter

object Repl {

  def read(line: String): Either[MalError, MalExpr] = MalParser(line)

  def eval(ast: MalExpr): Either[MalError, MalExpr] = Interpreter.evaluate(ast)

  def print(ast: MalExpr): String = AstPrinter.print(ast)

  def rep(line: String): Either[MalError, String] = for {
    ast <- read(line)
    result <- eval(ast)
  } yield {
    print(result)
  }

  def reportParseError(err: MalParseError): Unit = {
    println(s"Parse error: ${err.msg}")
    val listing = err.source.pos.longString
    val lines = listing.lines.toArray
    val lineno = s"${err.source.pos.line}| "
    lines(0) = s"$lineno${lines(0)}"
    lines(1) = s"${" " * lineno.length}${lines(1)}"
    println(lines.mkString("\n"))
  }

  def reportEvaluationError(err: MalEvaluationError): Unit = {
    println(s"Evaluation error: ${err.msg}")
    println(AstPrinter.print(err.node))
  }

  def reportError(err: MalError): Unit = {
    err match {
      case parseError: MalParseError => reportParseError(parseError)
      case evalError: MalEvaluationError => reportEvaluationError(evalError)
    }
  }

  def repl(): Unit = {
    val line = scala.io.StdIn.readLine("mal>")
    if (line == null) return
    if (line.isEmpty) repl()

    rep(line) match {
      case Left(err) => reportError(err)
      case Right(value) => println(value)
    }
    repl()
  }

  def main(args: Array[String]): Unit = {
    repl()
  }

}
