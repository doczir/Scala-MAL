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


  it should "contain basic conditionals" in {
    eval("(= 2 1)") shouldBe false.mb
    eval("(= 1 1)") shouldBe true.mb
    eval("(= 1 2)") shouldBe false.mb
    eval("(= 1 (+ 1 1))") shouldBe false.mb
    eval("(= 2 (+ 1 1))") shouldBe true.mb
    eval("(= nil 1)") shouldBe false.mb
    eval("(= nil nil)") shouldBe true.mb

    eval("(> 2 1)") shouldBe true.mb
    eval("(> 1 1)") shouldBe false.mb
    eval("(> 1 2)") shouldBe false.mb

    eval("(>= 2 1)") shouldBe true.mb
    eval("(>= 1 1)") shouldBe true.mb
    eval("(>= 1 2)") shouldBe false.mb

    eval("(< 2 1)") shouldBe false.mb
    eval("(< 1 1)") shouldBe false.mb
    eval("(< 1 2)") shouldBe true.mb

    eval("(<= 2 1)") shouldBe false.mb
    eval("(<= 1 1)") shouldBe true.mb
    eval("(<= 1 2)") shouldBe true.mb
  }

  it should "handle equality checks" in {
    eval("""(= 1 1)""") shouldBe true.mb
    eval("""(= 0 0)""") shouldBe true.mb
    eval("""(= 1 0)""") shouldBe false.mb
    eval("""(= "" "")""") shouldBe true.mb
    eval("""(= "abc" "abc")""") shouldBe true.mb
    eval("""(= "abc" "")""") shouldBe false.mb
    eval("""(= "" "abc")""") shouldBe false.mb
    eval("""(= "abc" "def")""") shouldBe false.mb
    eval("""(= "abc" "ABC")""") shouldBe false.mb

    eval("""(= (list) (list))""") shouldBe true.mb
    eval("""(= (list 1 2) (list 1 2))""") shouldBe true.mb
    eval("""(= (list 1) (list))""") shouldBe false.mb
    eval("""(= (list) (list 1))""") shouldBe false.mb
    eval("""(= 0 (list))""") shouldBe false.mb
    eval("""(= (list) 0)""") shouldBe false.mb
    eval("""(= (list) "")""") shouldBe false.mb
    eval("""(= "" (list))""") shouldBe false.mb
  }

}
