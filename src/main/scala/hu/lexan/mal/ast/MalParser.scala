package hu.lexan.mal.ast

import hu.lexan.mal.error.{MalError, MalParseError}

import scala.util.matching.Regex
import scala.util.parsing.combinator._

object MalParser extends RegexParsers {

  import MalAstExtensions._

  override def skipWhitespace: Boolean = true

  override val whiteSpace: Regex = """[\s,]+|;.*""".r

  def spaces: Parser[Unit] = {
    rep1("""[\s,]""".r) ^^ (_ => ())
  }

  def comment: Parser[Unit] = {
    """;.*""".r ^^ (_ => ())
  }

  def ignored: Parser[Unit] = {
    spaces | comment ^^ (_ => ())
  }

  def symbol: Parser[MalExpr] = {
    """[a-zA-Z!#$%&|*+\-/:<=>?@^_~][0-9a-zA-Z!#$%&|*+\-/:<=>?@^_~]*""".r ^^ (sym => sym.msym)
  }

  def mtrue: Parser[MTrue.type] = "true" ^^ (_ => MTrue)

  def mfalse: Parser[MFalse.type] = "false" ^^ (_ => MFalse)

  def mnil: Parser[MNil.type] = "nil" ^^ (_ => MNil)

  def integer: Parser[MalExpr] = {
    """-?[1-9][0-9]*""".r ^^ (int => int.toInt.mi)
  }

  def string: Parser[MalExpr] = {
    """"(?:\\.|[^\\"])*"""".r ^^ (str => MString(str.substring(1, str.length - 1)))
  }

  def keyword: Parser[MalExpr] = {
    """:[0-9a-zA-Z!#$%&|*+\-/:<=>?@^_~]*""".r ^^ (kw => MKeyword(kw.substring(1)))
  }

  def list: Parser[MalExpr] = {
    "(" ~> rep(expr) <~ ")" ^^ {
      nodes => MList(nodes)
    }
  }

  def vector: Parser[MalExpr] = {
    "[" ~> rep(expr) <~ "]" ^^ {
      nodes => MVector(nodes)
    }
  }

  def mmap: Parser[MalExpr] = {
    "{" ~> rep(expr) <~ "}" ^^ { 
      nodes => MMap(nodes.grouped(2).map { case (key :: value :: Nil) => (key, value) }.toMap)
    }
  }

  def atom: Parser[MalExpr] = {
    integer | string | mtrue | mfalse | mnil | keyword | symbol
  }

  def quote: Parser[MalExpr] = {
    "'" ~> expr ^^ {x => MList(List(MSymbol("quote"), x))}
  }

  def quasiquote: Parser[MalExpr] = {
    "`" ~> expr ^^ {x => MList(List(MSymbol("quasiquote"), x))}
  }

  def spliceUnquote: Parser[MalExpr] = {
    "~@" ~> expr ^^ {x => MList(List(MSymbol("splice-unquote"), x))}
  }

  def unquote: Parser[MalExpr] = {
    "~" ~> expr ^^ {x => MList(List(MSymbol("unquote"), x))}
  }

  def deref: Parser[MalExpr] = {
    "@" ~> expr ^^ {x => MList(List(MSymbol("deref"), x))}
  }

  def withMeta: Parser[MalExpr] = {
    "^" ~> expr ~ expr ^^ {case meta ~ x => MList(List(MSymbol("with-meta"), x, meta))}
  }

  def mmacro: Parser[MalExpr] = {
    quote | quasiquote | spliceUnquote | unquote | deref | withMeta
  }

  def expr: Parser[MalExpr] = {
    mmacro | list | vector | mmap | atom <~ opt(ignored)
  }

  def apply(input: String): Either[MalError, MalExpr] = {
    parse(phrase(expr), input) match {
      case NoSuccess(msg, source) => Left(MalParseError(msg, source))
      case Success(result, _) => Right(result)
    }
  }
}
