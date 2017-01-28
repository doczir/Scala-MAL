package hu.lexan.mal

import hu.lexan.mal.ast.{MalAstExtensions, MalExpr}
import org.scalatest.{FlatSpec, Matchers}

class MalEnvSpec extends FlatSpec with  Matchers {

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

  "The environment" should "contain prelude" in {
    eval("(+ 1 2)") shouldBe 3.mi
    eval("(/ (- (+ 5 (* 2 3)) 3) 4)") shouldBe 2.mi
  }

  it should "add elements with def!" in {
    eval("(def! x 3)") shouldBe 3.mi
    eval("x") shouldBe 3.mi
    eval("(def! x 4)") shouldBe 4.mi
    eval("x") shouldBe 4.mi
    eval("(def! y (+ 1 7))") shouldBe 8.mi
    eval("y") shouldBe 8.mi
  }

  it should "be case sensitive" in {
    eval("(def! mynum 111)") shouldBe 111.mi
    eval("(def! MYNUM 222)") shouldBe 222.mi
    eval("mynum") shouldBe 111.mi
    eval("MYNUM") shouldBe 222.mi
  }

  it should "work with let*" in {
    eval("(def! x 4)") shouldBe 4.mi
    eval("(let* (z 9) z)") shouldBe 9.mi
    eval("(let* (x 9) x)") shouldBe 9.mi
    eval("x") shouldBe 4.mi
    eval("(let* (z (+ 2 3)) (+ 1 z))") shouldBe 6.mi
    eval("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))") shouldBe 12.mi
    eval("(def! y (let* (z 7) z))") shouldBe 7.mi
    eval("y") shouldBe 7.mi
  }

  it should "work with outer environment" in {
    eval("(def! a 4)") shouldBe 4.mi
    eval("(let* (q 9) q)") shouldBe 9.mi
    eval("(let* (q 9) a)") shouldBe 4.mi
    eval("(let* (z 2) (let* (q 9) a))") shouldBe 4.mi
    eval("(let* (x 4) (def! a 5))") shouldBe 5.mi
    eval("a") shouldBe 4.mi
  }

  it should "work with let* with vector binding" in {
    eval("(let* [z 9] z)") shouldBe 9.mi
    eval("(let* (a 5 b 6) [3 4 a [b 7] 8])") shouldBe List(3.mi, 4.mi, 5.mi, List(6.mi, 7.mi).mv, 8.mi).mv
  }
}
