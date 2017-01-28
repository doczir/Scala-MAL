package hu.lexan.mal.test

import java.io.ByteArrayOutputStream

import hu.lexan.mal.Repl
import hu.lexan.mal.ast.{MNil, MalAstExtensions, MalExpr}
import org.scalatest.{FlatSpec, Matchers}

class ControlFlowSpec extends FlatSpec with Matchers {

  import MalAstExtensions._

  def eval(testInput: String): MalExpr = {
    val result = for {
      ast <- Repl.read(testInput)
      result <- Repl.eval(ast)
    } yield result
    if (result.isLeft)
      fail(s"${result.left.get.toString} '''$testInput'''")

    result.right.get
  }

  "The prelude" should "contain list functions" in {
    eval("(list)") shouldBe List().ml
    eval("(list? (list))") shouldBe true.mb
    eval("(empty? (list))") shouldBe true.mb
    eval("(empty? (list 1))") shouldBe false.mb
    eval("(list 1 2 3)") shouldBe List(1.mi, 2.mi, 3.mi).ml
    eval("(count (list 1 2 3))") shouldBe 3.mi
    eval("(count (list))") shouldBe 0.mi
    eval("(count nil)") shouldBe 0.mi
    eval("""(if (> (count (list 1 2 3)) 3) "yes" "no")""") shouldBe "no".mstr
    eval("""(if (>= (count (list 1 2 3)) 3) "yes" "no")""") shouldBe "yes".mstr
  }

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

}
