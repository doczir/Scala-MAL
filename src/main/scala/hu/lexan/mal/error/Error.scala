package hu.lexan.mal.error

import hu.lexan.mal.ast.{MalExpr, MalParser}

trait MalError

case class MalParseError(msg: String, source: MalParser.Input) extends MalError
case class MalEvaluationError(msg: String, node: MalExpr) extends MalError
case class MalCompositeError(errors: List[MalError]) extends MalError
