package hu.lexan.mal

import hu.lexan.mal.ast.{MalAstExtensions, MalExpr}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 1/23/17.
  */
class MalEvalSpec extends FlatSpec with Matchers {

  import MalAstExtensions._

  def eval(testInput: String): MalExpr = {
    val result = for {
      ast <- Repl.read(testInput)
      result <- Repl.eval(ast)
    } yield result
    if(result.isLeft)
      fail(s"${result.left.get.toString} '''$testInput'''")

    result.right.get
  }

  "The evaluator" should "evaluate simple arithmetic expressions" in {
    eval("(+ 1 2)") shouldBe 3.mi
    eval("(+ 5 (* 2 3))") shouldBe 11.mi
    eval("(- (+ 5 (* 2 3)) 3)") shouldBe 8.mi
    eval("(/ (- (+ 5 (* 2 3)) 3) 4)") shouldBe 2.mi
    eval("(/ (- (+ 515 (* 87 311)) 302) 27)") shouldBe 1010.mi
    eval("(* -3 6)") shouldBe -18.mi
    eval("(/ (- (+ 515 (* -87 311)) 296) 27)") shouldBe -994.mi
  }

  it should "handle empty list gracefully" in {
    eval("()") shouldBe List().ml
  }

  it should "evaluate within collection literals" in {
    eval("[1 2 (+ 1 2)]") shouldBe List(1.mi, 2.mi, 3.mi).mv
    eval("""{"a" (+ 7 8)}""") shouldBe Map[MalExpr, MalExpr](("a".mstr, 15.mi)).mm
    eval("""{:a (+ 7 8)}""") shouldBe Map[MalExpr, MalExpr](("a".mkw, 15.mi)).mm
  }

}
