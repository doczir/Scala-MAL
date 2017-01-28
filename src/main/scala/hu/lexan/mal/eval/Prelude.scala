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
    println(args.map(AstPrinter.print(_)).mkString(" "))
    Right(MNil)
  }

  private def prnln(args: List[MalExpr]): Either[MalError, MalExpr] = {
    println(args.map(AstPrinter.print(_, readable = false)).mkString(" "))
    Right(MNil)
  }

  private def pr_str(args: List[MalExpr]): Either[MalError, MalExpr] = {
    Right(MString(args.map(AstPrinter.print(_)).mkString(" ")))
  }

  private def str(args: List[MalExpr]): Either[MalError, MalExpr] = {
    Right(MString(args.map(AstPrinter.print(_, readable = false)).mkString(" ")))
  }

  private def list(args: List[MalExpr]): Either[MalError, MalExpr] = {
    Right(MList(args))
  }

  private def list_test(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case (_: MList) :: _ => Right(MTrue)
    case _ => Right(MFalse)
  }

  private def empty_test(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MList(elements) :: _ if elements.isEmpty => Right(MTrue)
    case _ => Right(MFalse)
  }

  private def count(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MList(elements) :: _ => Right(elements.length.mi)
    case _ => Right(0.mi)
  }

  private def equal_test(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case a1 :: a2 :: _ => Right((a1 == a2).mb)
    case _ => Right(MFalse)
  }

  private def lt(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MInteger(a1) :: MInteger(a2) :: Nil => Right((a1 < a2).mb)
    case _ => Left(MalEvaluationError("Illegal arguments to <", MList("<".msym :: args)))
  }

  private def gt(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MInteger(a1) :: MInteger(a2) :: Nil => Right((a1 > a2).mb)
    case _ => Left(MalEvaluationError("Illegal arguments to >", MList(">".msym :: args)))
  }

  private def lte(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MInteger(a1) :: MInteger(a2) :: Nil => Right((a1 <= a2).mb)
    case _ => Left(MalEvaluationError("Illegal arguments to <=", MList("<=".msym :: args)))
  }

  private def gte(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MInteger(a1) :: MInteger(a2) :: Nil => Right((a1 >= a2).mb)
    case _ => Left(MalEvaluationError("Illegal arguments to >=", MList(">=".msym :: args)))
  }

  private def not(args: List[MalExpr]): Either[MalError, MalExpr] = args match {
    case MFalse :: Nil => Right(MTrue)
    case MNil :: Nil => Right(MTrue)
    case _ => Right(MFalse)
  }



  val env: Environment = {
    val env = new Environment()

    env.define("+".msym, MFunction(add))
    env.define("-".msym, MFunction(sub))
    env.define("*".msym, MFunction(mul))
    env.define("/".msym, MFunction(div))
    env.define("pr-str".msym, MFunction(pr_str))
    env.define("str".msym, MFunction(str))
    env.define("prn".msym, MFunction(prn))
    env.define("println".msym, MFunction(prnln))
    env.define("list".msym, MFunction(list))
    env.define("list?".msym, MFunction(list_test))
    env.define("empty?".msym, MFunction(empty_test))
    env.define("count".msym, MFunction(count))
    env.define("=".msym, MFunction(equal_test))
    env.define("<".msym, MFunction(lt))
    env.define(">".msym, MFunction(gt))
    env.define("<=".msym, MFunction(lte))
    env.define(">=".msym, MFunction(gte))
    env.define("not".msym, MFunction(not))
    env
  }


}
