package hu.lexan.mal.test

import hu.lexan.mal.ast.{MNil, MalAstExtensions}
import org.scalatest.{FlatSpec, Matchers}

class TailRec extends FlatSpec with Matchers with ReplEvaluator {

  import MalAstExtensions._

  "Tailrec functions" should "be optimized" in {
    eval("(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))")
    eval("(sum2 10 0)") shouldBe 55.mi
    eval("(def! res2 nil)") shouldBe MNil
    eval("(def! res2 (sum2 10000 0))") shouldBe 50005000.mi
    eval("(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))")
    eval("(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))")
    eval("(foo 10000)") shouldBe 0.mi
  }

}
