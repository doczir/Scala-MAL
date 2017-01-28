package hu.lexan.mal.test

import java.io.ByteArrayOutputStream

import hu.lexan.mal.ast.{MNil, MTrue, MalAstExtensions}
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

  it should "be case sensitive" in {
    eval("""(def! DO (fn* (a) 7))""")
    eval("""(DO 3)""") shouldBe 7.mi
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

  it should "handle clojures" in {
    eval("""((fn* (a b) (+ b a)) 3 4)""") shouldBe 7.mi
    eval("""((fn* [a b] (+ b a)) 3 4)""") shouldBe 7.mi
    eval("""((fn* () 4))""") shouldBe 4.mi
    eval("""((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)""") shouldBe 8.mi
    eval("""( ( (fn* (a) (fn* (b) (+ a b))) 5) 7)""") shouldBe 12.mi

    eval("""(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))""")
    eval("""(def! plus5 (gen-plus5))""")
    eval("""(plus5 7)""") shouldBe 12.mi

    eval("""(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))""")
    eval("""(def! plus7 (gen-plusX 7))""")
    eval("""(plus7 8)""") shouldBe 15.mi
  }

  it should "handle recursive fibonacchi" in {
    eval("""(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))""")
    eval("(fib 1)") shouldBe 1.mi
    eval("(fib 2)") shouldBe 2.mi
    eval("(fib 4)") shouldBe 5.mi
    eval("(fib 10)") shouldBe 89.mi
  }

  it should "handle variable length args" in {
    eval("""( (fn* (& more) (count more)) 1 2 3)""") shouldBe 3.mi
    eval("""( (fn* (& more) (list? more)) 1 2 3)""") shouldBe MTrue
    eval("""( (fn* (& more) (count more)) 1)""") shouldBe 1.mi
    eval("""( (fn* (& more) (count more)) )""") shouldBe 0.mi
    eval("""( (fn* (& more) (list? more)) )""") shouldBe MTrue
    eval("""( (fn* (a & more) (count more)) 1 2 3)""") shouldBe 2.mi
    eval("""( (fn* (a & more) (count more)) 1)""") shouldBe 0.mi
    eval("""( (fn* (a & more) (list? more)) 1)""") shouldBe MTrue
  }
}
