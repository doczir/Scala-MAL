package hu.lexan.mal.eval

import hu.lexan.mal.ast.{MSymbol, MalExpr}
import hu.lexan.mal.error.{MalError, MalEvaluationError}

import scala.collection.mutable

class Environment(outer: Option[Environment] = None, symbolTable: mutable.Map[MSymbol, MalExpr] = mutable.Map()) {

  def define(sym: MSymbol, expr: MalExpr): Unit = {
    symbolTable(sym) = expr
  }

  def lookup(sym: MSymbol): Option[MalExpr] = symbolTable.get(sym).orElse {
    outer.flatMap(_.lookup(sym))
  }

  def get(sym: MSymbol): Either[MalError, MalExpr] = lookup(sym).toRight(MalEvaluationError(s"Symbol '${sym.name}' not found in environment", sym))

}
