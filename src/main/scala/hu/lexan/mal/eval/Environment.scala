package hu.lexan.mal.eval

import hu.lexan.mal.ast.{MList, MSymbol, MalExpr}
import hu.lexan.mal.error.{MalError, MalEvaluationError}

import scala.collection.mutable

class Environment(outer: Option[Environment] = None, private val symbolTable: mutable.Map[MSymbol, MalExpr] = mutable.Map()) {

  def define(sym: MSymbol, expr: MalExpr): Unit = {
    symbolTable(sym) = expr
  }

  def lookup(sym: MSymbol): Option[MalExpr] = symbolTable.get(sym).orElse {
    outer.flatMap(_.lookup(sym))
  }

  def get(sym: MSymbol): Either[MalError, MalExpr] = lookup(sym).toRight(MalEvaluationError(s"Symbol '${sym.name}' not found in environment", sym))

}

object Environment {
  def bind(outer: Option[Environment], names: List[MalExpr], exprs: List[MalExpr]): Environment = {
    case class Iterator(exps: List[MalExpr], map: mutable.Map[MSymbol, MalExpr])
    val namesIt = names.iterator
    val resultEnv = namesIt.foldLeft(Right(Iterator(exprs, mutable.Map())): Either[MalEvaluationError, Iterator]) {
      case (Right(it), nameSym@MSymbol(name)) =>
        // TODO: handle when there are not enough expressions
        if (name == "&") namesIt.next() match {
          case nextName: MSymbol => Right(Iterator(Nil, it.map + ((nextName, MList(it.exps)))))
          case nextName => Left(MalEvaluationError("Name is not a symbol", nextName))
        } else Right(Iterator(it.exps.tail, it.map + ((nameSym, it.exps.head))))
      case (_, name) => Left(MalEvaluationError("Name is not a symbol", name))
    }.map(it => new Environment(outer, it.map))
    if (resultEnv.isLeft)
      throw resultEnv.left.get
    resultEnv.right.get
  }
}