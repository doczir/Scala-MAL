package hu.lexan.mal.test

import hu.lexan.mal.ast.MalAstExtensions
import org.scalatest.{FlatSpec, Matchers}

class PreludeSpec extends FlatSpec with Matchers with ReplEvaluator {

  import MalAstExtensions._

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


}
