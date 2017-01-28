package hu.lexan.mal.test

import java.io.ByteArrayOutputStream

import hu.lexan.mal.ast.{MFalse, MTrue, MalAstExtensions}
import org.scalatest.{FlatSpec, Matchers}

class PreludeSpec extends FlatSpec with Matchers with ReplEvaluator {

  import MalAstExtensions._

  "The prelude" should "contain list functions" in {
    eval("(list)") shouldBe List().ml
    eval("(list? (list))") shouldBe MTrue
    eval("(empty? (list))") shouldBe MTrue
    eval("(empty? (list 1))") shouldBe MFalse
    eval("(list 1 2 3)") shouldBe List(1.mi, 2.mi, 3.mi).ml
    eval("(count (list 1 2 3))") shouldBe 3.mi
    eval("(count (list))") shouldBe 0.mi
    eval("(count nil)") shouldBe 0.mi
    eval("""(if (> (count (list 1 2 3)) 3) "yes" "no")""") shouldBe "no".mstr
    eval("""(if (>= (count (list 1 2 3)) 3) "yes" "no")""") shouldBe "yes".mstr
  }


  it should "contain basic conditionals" in {
    eval("(= 2 1)") shouldBe MFalse
    eval("(= 1 1)") shouldBe MTrue
    eval("(= 1 2)") shouldBe MFalse
    eval("(= 1 (+ 1 1))") shouldBe MFalse
    eval("(= 2 (+ 1 1))") shouldBe MTrue
    eval("(= nil 1)") shouldBe MFalse
    eval("(= nil nil)") shouldBe MTrue

    eval("(> 2 1)") shouldBe MTrue
    eval("(> 1 1)") shouldBe MFalse
    eval("(> 1 2)") shouldBe MFalse

    eval("(>= 2 1)") shouldBe MTrue
    eval("(>= 1 1)") shouldBe MTrue
    eval("(>= 1 2)") shouldBe MFalse

    eval("(< 2 1)") shouldBe MFalse
    eval("(< 1 1)") shouldBe MFalse
    eval("(< 1 2)") shouldBe MTrue

    eval("(<= 2 1)") shouldBe MFalse
    eval("(<= 1 1)") shouldBe MTrue
    eval("(<= 1 2)") shouldBe MTrue
  }

  it should "handle equality checks" in {
    eval("""(= 1 1)""") shouldBe MTrue
    eval("""(= 0 0)""") shouldBe MTrue
    eval("""(= 1 0)""") shouldBe MFalse
    eval("""(= "" "")""") shouldBe MTrue
    eval("""(= "abc" "abc")""") shouldBe MTrue
    eval("""(= "abc" "")""") shouldBe MFalse
    eval("""(= "" "abc")""") shouldBe MFalse
    eval("""(= "abc" "def")""") shouldBe MFalse
    eval("""(= "abc" "ABC")""") shouldBe MFalse

    eval("""(= (list) (list))""") shouldBe MTrue
    eval("""(= (list 1 2) (list 1 2))""") shouldBe MTrue
    eval("""(= (list 1) (list))""") shouldBe MFalse
    eval("""(= (list) (list 1))""") shouldBe MFalse
    eval("""(= 0 (list))""") shouldBe MFalse
    eval("""(= (list) 0)""") shouldBe MFalse
    eval("""(= (list) "")""") shouldBe MFalse
    eval("""(= "" (list))""") shouldBe MFalse
  }

  it should "contain not" in {
    eval("""(not false)""") shouldBe MTrue
    eval("""(not true)""") shouldBe MFalse
    eval("""(not "a")""") shouldBe MFalse
    eval("""(not 0)""") shouldBe MFalse
  }

  it should "handle string functions" in {

    eval("""""""") shouldBe "".mstr
    eval(""""abc"""") shouldBe "abc".mstr
    eval(""""abc  def"""") shouldBe "abc  def".mstr
    eval(""""\""""") shouldBe """\"""".mstr
    eval(""""abc\ndef\nghi"""") shouldBe """abc\ndef\nghi""".mstr
    eval(""""abc\\ndef\\nghi"""") shouldBe """abc\\ndef\\nghi""".mstr

    eval("""(pr-str)""") shouldBe "".mstr
    eval("""(pr-str "abc")""") shouldBe """"abc"""".mstr
    eval("""(pr-str "abc def" "ghi jkl")""") shouldBe """"abc def" "ghi jkl"""".mstr
    eval("""(pr-str "\"")""") shouldBe """"\\\""""".mstr
    eval("""(pr-str (list 1 2 "abc" "\"") "def")""") shouldBe """(1 2 "abc" "\\\"") "def"""".mstr
    eval("""(pr-str (list))""") shouldBe """()""".mstr

    eval("""(str)""") shouldBe "".mstr
    eval("""(str "abc")""") shouldBe """abc""".mstr
    eval("""(str "abc def" "ghi jkl")""") shouldBe """abc def ghi jkl""".mstr
    eval("""(str "\"")""") shouldBe """\"""".mstr
    eval("""(str (list 1 2 "abc" "\"") "def")""") shouldBe """(1 2 abc \") def""".mstr
    eval("""(str (list))""") shouldBe """()""".mstr

    val outContent = new ByteArrayOutputStream()
    Console.withOut(outContent) {
      eval("""(prn)""")
      outContent.toString() shouldBe "\n"
      outContent.reset()
      eval("""(prn "abc")""")
      outContent.toString() shouldBe "\"abc\"\n"
      outContent.reset()
      eval("""(prn "abc def" "ghi jkl")""")
      outContent.toString() shouldBe "\"abc def\" \"ghi jkl\"\n"
      outContent.reset()
      eval("""(prn "\"")""")
      outContent.toString() shouldBe "\"\\\\\\\"\"\n"
      outContent.reset()
      eval("""(prn (list 1 2 "abc" "\"") "def")""")
      outContent.toString() shouldBe "(1 2 \"abc\" \"\\\\\\\"\") \"def\"\n"
      outContent.reset()
      eval("""(prn (list))""")
      outContent.toString() shouldBe "()\n"
      outContent.reset()
    }
  }
}
