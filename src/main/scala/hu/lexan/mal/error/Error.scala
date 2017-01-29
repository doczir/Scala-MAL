package hu.lexan.mal.error

import hu.lexan.mal.ast.{MNil, MalExpr, MalParser}

trait MalError extends Throwable

case class MalParseError(msg: String, source: MalParser.Input) extends MalError

case class MalEvaluationError(msg: String, node: MalExpr = MNil) extends MalError
case class MalCompositeError(errors: List[MalError]) extends MalError
