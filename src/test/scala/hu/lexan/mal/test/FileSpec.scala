package hu.lexan.mal.test

import java.io.ByteArrayOutputStream

import hu.lexan.mal.ast._
import org.scalatest.{FlatSpec, Matchers}


class FileSpec extends FlatSpec with Matchers with ReplEvaluator {

  import hu.lexan.mal.ast.MalAstExtensions._

  "The prelude" should "contain read-string, eval and slurp" in {
    eval("(read-string \"(1 2 (3 4) nil)\")") shouldBe List(1.mi, 2.mi, List(3.mi, 4.mi).ml, MNil).ml
    eval("(read-string \"(+ 2 3)\")") shouldBe List("+".msym, 2.mi, 3.mi).ml
    eval("(read-string \"7 ;; comment\")") shouldBe 7.mi

    eval("(eval (read-string \"(+ 2 3)\"))") shouldBe 5.mi
    eval("(slurp \"src/test/resources/test.txt\")") shouldBe "A line of test".mstr

    eval("(load-file \"src/test/resources/inc.mal\")")
    eval("(inc1 7)") shouldBe 8.mi
    eval("(inc2 7)") shouldBe 9.mi
    eval("(inc3 9)") shouldBe 12.mi

    val outContent = new ByteArrayOutputStream()
    Console.withOut(outContent) {
      eval("(load-file \"src/test/resources/incB.mal\")") shouldBe "incB.mal return string".mstr
      outContent.toString() == "incB.mal finished"
      outContent.reset()
    }
    eval("(inc4 7)") shouldBe 11.mi
    eval("(inc5 7)") shouldBe 12.mi

    eval("(load-file \"src/test/resources/incC.mal\")")
    eval("mymap") shouldBe Map[MalExpr, MalExpr]("a".mstr -> 1.mi).mm

  }

}
