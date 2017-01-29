package hu.lexan.mal.test

import hu.lexan.mal.ast.{MFalse, MTrue}
import org.scalatest.{FlatSpec, Matchers}

class AtomTests extends FlatSpec with Matchers with ReplEvaluator {

  import hu.lexan.mal.ast.MalAstExtensions._

  "The prelude" should "handle atoms" in {
    eval("(def! inc3 (fn* (a) (+ 3 a)))")

    eval("(def! a (atom 2))") shouldBe 2.mi.a
    eval("(atom? a)") shouldBe MTrue
    eval("(atom? 1)") shouldBe MFalse
    eval("(deref a)") shouldBe 2.mi
    eval("(reset! a 3)") shouldBe 3.mi
    eval("(deref a)") shouldBe 3.mi
    eval("(swap! a inc3)") shouldBe 6.mi
    eval("(deref a)") shouldBe 6.mi
    eval("(swap! a (fn* (a) a))") shouldBe 6.mi
    eval("(swap! a (fn* (a) (* 2 a)))") shouldBe 12.mi
    eval("(swap! a (fn* (a b) (* a b)) 10)") shouldBe 120.mi
    eval("(swap! a + 3)") shouldBe 123.mi

    eval("(def! inc-it (fn* (a) (+ 1 a)))")
    eval("(def! atm (atom 7))")
    eval("(def! f (fn* () (swap! atm inc-it)))")
    eval("(f)") shouldBe 8.mi
    eval("(f)") shouldBe 9.mi

  }
}
