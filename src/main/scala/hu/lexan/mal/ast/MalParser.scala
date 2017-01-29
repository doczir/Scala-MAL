package hu.lexan.mal.ast

import hu.lexan.mal.error.{MalError, MalParseError}

import scala.util.matching.Regex
import scala.util.parsing.combinator._

object MalParser extends RegexParsers {

  import MalAstExtensions._

  override def skipWhitespace: Boolean = true

  override val whiteSpace: Regex = """([\s,]+|;.*)+""".r

  private def spaces: Parser[Unit] = {
    rep1("""[\s,]""".r) ^^ (_ => ())
  }

  private def comment: Parser[Unit] = {
    """;.*""".r ^^ (_ => ())
  }

  private def ignored: Parser[Unit] = {
    spaces | comment ^^ (_ => ())
  }

  private def symbol: Parser[MalExpr] = {
    """[a-zA-Z!#$%&|*+\-/:<=>?@^_~][0-9a-zA-Z!#$%&|*+\-/:<=>?@^_~]*""".r ^^ (sym => sym.msym)
  }

  private def mtrue: Parser[MTrue.type] = "true" ^^ (_ => MTrue)

  private def mfalse: Parser[MFalse.type] = "false" ^^ (_ => MFalse)

  private def mnil: Parser[MNil.type] = "nil" ^^ (_ => MNil)

  private def integer: Parser[MalExpr] = {
    """-?[0-9]+""".r ^^ (int => int.toInt.mi)
  }

  private def string: Parser[MalExpr] = {
    """"(?:\\.|[^\\"])*"""".r ^^ (str => MString(str.substring(1, str.length - 1)))
  }

  private def keyword: Parser[MalExpr] = {
    """:[0-9a-zA-Z!#$%&|*+\-/:<=>?@^_~]*""".r ^^ (kw => MKeyword(kw.substring(1)))
  }

  private def list: Parser[MalExpr] = {
    "(" ~> rep(expr) <~ ")" ^^ {
      nodes => MList(nodes)
    }
  }

  private def vector: Parser[MalExpr] = {
    "[" ~> rep(expr) <~ "]" ^^ {
      nodes => MVector(nodes)
    }
  }

  private def mmap: Parser[MalExpr] = {
    "{" ~> rep(expr) <~ "}" ^^ { 
      nodes => MMap(nodes.grouped(2).map { case (key :: value :: Nil) => (key, value) }.toMap)
    }
  }

  private def atom: Parser[MalExpr] = {
    integer | string | mtrue | mfalse | mnil | keyword | symbol
  }

  private def quote: Parser[MalExpr] = {
    "'" ~> expr ^^ {x => MList(List(MSymbol("quote"), x))}
  }

  private def quasiquote: Parser[MalExpr] = {
    "`" ~> expr ^^ {x => MList(List(MSymbol("quasiquote"), x))}
  }

  private def spliceUnquote: Parser[MalExpr] = {
    "~@" ~> expr ^^ {x => MList(List(MSymbol("splice-unquote"), x))}
  }

  private def unquote: Parser[MalExpr] = {
    "~" ~> expr ^^ {x => MList(List(MSymbol("unquote"), x))}
  }

  private def deref: Parser[MalExpr] = {
    "@" ~> expr ^^ {x => MList(List(MSymbol("deref"), x))}
  }

  private def withMeta: Parser[MalExpr] = {
    "^" ~> expr ~ expr ^^ {case meta ~ x => MList(List(MSymbol("with-meta"), x, meta))}
  }

  private def mmacro: Parser[MalExpr] = {
    quote | quasiquote | spliceUnquote | unquote | deref | withMeta
  }

  private def expr: Parser[MalExpr] = {
    mmacro | list | vector | mmap | atom <~ opt(ignored)
  }

  // TODO: fix error when comment is at line end
  def apply(input: String): Either[MalError, MalExpr] = {
    parse(phrase(expr), input) match {
      case NoSuccess(msg, source) => Left(MalParseError(msg, source))
      case Success(result, _) => Right(result)
    }
  }
}
