package hu.lexan.mal.ast

import hu.lexan.mal.error.MalError

// ===================================
//                AST
// ===================================
sealed trait MalExpr extends Any

case class MSymbol(name: String) extends AnyVal with MalExpr

case class MInteger(value: Int) extends AnyVal with MalExpr

case class MList(elements: List[MalExpr]) extends AnyVal with MalExpr

case class MVector(elements: List[MalExpr]) extends AnyVal with MalExpr

case class MMap(elements: Map[MalExpr, MalExpr]) extends AnyVal with MalExpr

case class MString(string: String) extends AnyVal with MalExpr

case class MKeyword(string: String) extends AnyVal with MalExpr

case class MFunction(fn: (List[MalExpr] => Either[MalError, MalExpr])) extends AnyVal with MalExpr

object MNil extends MalExpr

object MTrue extends MalExpr

object MFalse extends MalExpr

object MalAstExtensions {

  implicit class RichMalString(val s: String) extends AnyVal {
    def msym = MSymbol(s)

    def mstr = MString(s)

    def mkw = MKeyword(s)
  }

  implicit class RichMalInteger(val s: Int) extends AnyVal {
    def mi = MInteger(s)
  }

  implicit class RichMalList(val s: List[MalExpr]) extends AnyVal {
    def ml = MList(s)

    def mv = MVector(s)
  }

  implicit class RichMalMap(val s: Map[MalExpr, MalExpr]) extends AnyVal {
    def mm = MMap(s)
  }

  implicit class RichMalBool(val s: Boolean) extends AnyVal {
    def mb: MalExpr = if (s) MTrue else MFalse
  }
}

// ===================================
//                UTILS
// ===================================
object AstPrinter {
  private def unescape(c: Char): String = c match {
    case '\n' => "\\n"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case anychar => anychar.toString
  }

  def print(expr: MalExpr, readable: Boolean = true): String = expr match {
    case MSymbol(name) => name
    case MKeyword(kw) => s":$kw"
    case MString(str) => if (readable) s""""${str.flatMap(unescape)}"""" else str
    case MInteger(value) => value.toString
    case MList(nodes) => s"(${
      nodes.map {
        print(_, readable)
      }.mkString(" ")
    })"
    case MVector(nodes) => s"[${
      nodes.map {
        print(_, readable)
      }.mkString(" ")
    }]"
    case MMap(elems) => s"{${
      elems.toList.map { case (key, value) => s"${print(key)} ${print(value)}" }.mkString(" ")
    }}"
    case MNil => "nil"
    case MTrue => "true"
    case MFalse => "false"
    case _: MFunction => "#<function>"
  }
}
