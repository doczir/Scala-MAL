package hu.lexan.mal.test

import java.io.ByteArrayOutputStream

import hu.lexan.mal.ast.{MNil, MalAstExtensions}
import org.scalatest.{FlatSpec, Matchers}

class ExpressionSpec extends FlatSpec with Matchers with ReplEvaluator {

  import MalAstExtensions._

  "The repl" should "handle do expressions" in {
    val outContent = new ByteArrayOutputStream()
    Console.withOut(outContent) {
      eval("""(do (prn "prn output1"))""") shouldBe MNil
      outContent.toString() shouldBe "\"prn output1\"\n"
      outContent.reset()

      eval("""(do (prn "prn output2") 7)""") shouldBe 7.mi
      outContent.toString() shouldBe "\"prn output2\"\n"
      outContent.reset()

      eval("""(do (prn "prn output1") (prn "prn output2") (+ 1 2))""") shouldBe 3.mi
      outContent.toString() shouldBe "\"prn output1\"\n\"prn output2\"\n"
      outContent.reset()

      eval("""(do (def! a 6) 7 (+ a 8))""") shouldBe 14.mi
      eval("a") shouldBe 6.mi
    }
  }

  it should "handle if expressions" in {
    eval("(if true 7 8)") shouldBe 7.mi
    eval("(if false 7 8)") shouldBe 8.mi
    eval("""(if true (+ 1 7) (+ 1 8))""") shouldBe 8.mi
    eval("""(if false (+ 1 7) (+ 1 8))""") shouldBe 9.mi
    eval("""(if nil 7 8)""") shouldBe 8.mi
    eval("""(if 0 7 8)""") shouldBe 7.mi
    eval("""(if "" 7 8)""") shouldBe 7.mi
    eval("""(if (list) 7 8)""") shouldBe 7.mi
    eval("""(if (list 1 2 3) 7 8)""") shouldBe 7.mi
    eval("""(= (list) nil)""") shouldBe false.mb
  }

  it should "allow for one way if" in {
    eval("(if false (+ 1 7))") shouldBe MNil
    eval("(if nil 8 7)") shouldBe 7.mi
    eval("(if true (+ 1 7))") shouldBe 8.mi
  }


}
