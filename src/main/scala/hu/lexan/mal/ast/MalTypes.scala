package hu.lexan.mal.ast

import hu.lexan.mal.error.MalError
import hu.lexan.mal.eval.Environment

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

case class MFunction(fn: (List[MalExpr] => Either[MalError, MalExpr])) extends MalExpr

case class MClojure(fn: (List[MalExpr] => Either[MalError, MalExpr]),
                    ast: MalExpr,
                    env: Environment,
                    params: MalExpr) extends MalExpr

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


