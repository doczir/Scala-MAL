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

  }


}
