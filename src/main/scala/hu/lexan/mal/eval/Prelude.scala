package hu.lexan.mal.eval

import hu.lexan.mal.ast._
import hu.lexan.mal.error.{MalError, MalEvaluationError}

object Prelude {

  import MalAstExtensions._

  private def add(args: List[MalExpr]) = args match {
    case (a: MInteger) :: (b: MInteger) :: Nil => Right(MInteger(a.value + b.value))
    case _ => Left(MalEvaluationError("Illegal arguments to +", MList("+".msym :: args)))
  }

  private def sub(args: List[MalExpr]) = args match {
    case (a: MInteger) :: (b: MInteger) :: Nil => Right(MInteger(a.value - b.value))
    case _ => Left(MalEvaluationError("Illegal arguments to -", MList("-".msym :: args)))
  }

  private def mul(args: List[MalExpr]) = args match {
    case (a: MInteger) :: (b: MInteger) :: Nil => Right(MInteger(a.value * b.value))
    case _ => Left(MalEvaluationError("Illegal arguments to *", MList("*".msym :: args)))
  }

  private def div(args: List[MalExpr]) = args match {
    case (a: MInteger) :: (b: MInteger) :: Nil => Right(MInteger(a.value / b.value))
    case _ => Left(MalEvaluationError("Illegal arguments to /", MList("/".msym :: args)))
  }

  private def prn(args: List[MalExpr]): Either[MalError, MalExpr] = {
    println(args.map(AstPrinter.print).mkString(" "))
    Right(MNil)
  }

  private def list(args: List[MalExpr]): Either[MalError, MalExpr] = {
    Right(MList(args))
  }

  val env: Environment = {
    val env = new Environment()

    env.define("+".msym, MFunction(add))
    env.define("-".msym, MFunction(sub))
    env.define("*".msym, MFunction(mul))
    env.define("/".msym, MFunction(div))
    env.define("list".msym, MFunction(list))
    env.define("prn".msym, MFunction(prn))
    env
  }


}
