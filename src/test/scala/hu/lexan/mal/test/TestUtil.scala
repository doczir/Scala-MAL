package hu.lexan.mal.test

import hu.lexan.mal.Repl
import hu.lexan.mal.ast.MalExpr
import org.scalatest.Matchers

trait ReplEvaluator extends Matchers {

  def eval(testInput: String): MalExpr = {
    val result = for {
      ast <- Repl.read(testInput)
      result <- Repl.eval(ast)
    } yield result
    if (result.isLeft)
      fail(s"${result.left.get.toString} '''$testInput'''")

    result.right.get
  }
}