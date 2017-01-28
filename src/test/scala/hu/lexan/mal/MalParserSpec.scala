package hu.lexan.mal

import hu.lexan.mal.ast._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 1/21/17.
  */
class MalParserSpec extends FlatSpec with Matchers {

  import MalAstExtensions._

  def parse(testInput: String): MalExpr = {
    val eitherAst = Repl.read(testInput)
    if(eitherAst.isLeft)
      fail(eitherAst.left.get.toString)

    eitherAst.right.get
  }

  "The parser" should "read numbers" in {
    parse("1") shouldBe 1.mi
    parse(",,,1") shouldBe 1.mi
    parse("7") shouldBe 7.mi
    parse("    7") shouldBe 7.mi
    parse("-123") shouldBe (-123.mi)
  }

  it should "read symbols" in {
    parse("+") shouldBe "+".msym
    parse("abc") shouldBe "abc".msym
    parse("    abc") shouldBe "abc".msym
    parse("abc5") shouldBe "abc5".msym
    parse("abc-def") shouldBe "abc-def".msym
  }

  it should "read lists" in {
    parse("(+ 1 2)") shouldBe List("+".msym, 1.mi, 2.mi).ml
    parse("()") shouldBe List().ml
    parse("(nil)") shouldBe List(MNil).ml
    parse("((3 4))") shouldBe List(List(3.mi, 4.mi).ml).ml
    parse("(+ 1 (+ 2 3))") shouldBe List("+".msym, 1.mi, List("+".msym, 2.mi, 3.mi).ml).ml
    parse("  ( +   1   (+   2 3   )   )  ") shouldBe List("+".msym, 1.mi, List("+".msym, 2.mi, 3.mi).ml).ml
    parse("(* 1 2)") shouldBe List("*".msym, 1.mi, 2.mi).ml
    parse("(** 1 2)") shouldBe List("**".msym, 1.mi, 2.mi).ml
    parse("(* -3 6)") shouldBe List("*".msym, -3.mi, 6.mi).ml
  }

  it should "treat ',' as whitespace" in {
    parse("(1 2, 3,,,,)") shouldBe List(1.mi, 2.mi, 3.mi).ml
    parse("()") shouldBe List().ml
  }

  it should "read nil, true and false" in {
    parse("nil") shouldBe MNil
    parse("true") shouldBe MTrue
    parse("false") shouldBe MFalse
  }

  it should "read strings" in {
    parse(""""abc"""") shouldBe "abc".mstr
    parse("""    "abc"""") shouldBe "abc".mstr
    parse(""""abc (with parens)"""") shouldBe "abc (with parens)".mstr
    parse(""""abc\"def"""") shouldBe """abc\"def""".mstr
    parse(""""abc\ndef"""") shouldBe """abc\ndef""".mstr
    parse("""""""") shouldBe "".mstr
  }

  it should "read vectors" in {
    parse("[+ 1 2]") shouldBe List("+".msym, 1.mi, 2.mi).mv
    parse("[]") shouldBe List().mv
    parse("[nil]") shouldBe List(MNil).mv
    parse("[[3 4]]") shouldBe List(List(3.mi, 4.mi).mv).mv
    parse("[+ 1 [+ 2 3]]") shouldBe List("+".msym, 1.mi, List("+".msym, 2.mi, 3.mi).mv).mv
    parse("  [ +   1   [+   2 3   ]   ]  ") shouldBe List("+".msym, 1.mi, List("+".msym, 2.mi, 3.mi).mv).mv
    parse("[* 1 2]") shouldBe List("*".msym, 1.mi, 2.mi).mv
    parse("[** 1 2]") shouldBe List("**".msym, 1.mi, 2.mi).mv
    parse("[* -3 6]") shouldBe List("*".msym, -3.mi, 6.mi).mv
  }

  it should "read maps" in {
    parse("""{"abc" 1}""") shouldBe Map[MalExpr, MalExpr](("abc".mstr, 1.mi)).mm
    parse("""{"a" {"b" 1}}""") shouldBe Map[MalExpr, MalExpr](("a".mstr, Map[MalExpr, MalExpr](("b".mstr, 1.mi)).mm)).mm
  }

  it should "read keywords" in {
    parse(":kw") shouldBe "kw".mkw
    parse("(:kw1 :kw2)") shouldBe List("kw1".mkw, "kw2".mkw).ml
  }

  it should "ignore comments" in {
    parse("1 ; comment") shouldBe 1.mi
    parse("1; comment") shouldBe 1.mi
  }

  it should "read quote" in {
    parse("'1") shouldBe List("quote".msym, 1.mi).ml
    parse("'(1 2 3)") shouldBe List("quote".msym, List(1.mi, 2.mi, 3.mi).ml).ml
    parse("`1") shouldBe List("quasiquote".msym, 1.mi).ml
    parse("~1") shouldBe List("unquote".msym, 1.mi).ml
    parse("~(1 2 3)") shouldBe List("unquote".msym, List(1.mi, 2.mi, 3.mi).ml).ml
    parse("`(1 ~a 3)") shouldBe List("quasiquote".msym, List(1.mi, List("unquote".msym, "a".msym).ml, 3.mi).ml).ml
    parse("~@(1 2 3)") shouldBe List("splice-unquote".msym, List(1.mi, 2.mi, 3.mi).ml).ml
  }

  it should "read metadata" in {
    parse("""^{"a" 1} [1 2 3]""") shouldBe List("with-meta".msym, List(1.mi, 2.mi, 3.mi).mv, Map[MalExpr, MalExpr](("a".mstr, 1.mi)).mm).ml
  }
}
