package hu.lexan.mal.eval

import hu.lexan.mal.ast.{MalExpr, _}
import hu.lexan.mal.error.MalEvaluationError

import scala.io.Source

object Prelude {

  import MalAstExtensions._

  type MalOp = PartialFunction[List[MalExpr], MalExpr]
  type BinOp = PartialFunction[(MalExpr, MalExpr), MalExpr]

  def chain[Op <: PartialFunction[In, Out], In, Out](ops: Op*): PartialFunction[In, Out] = (PartialFunction.empty[In, Out] /: ops) (_ orElse _)

  def chainBinOp(ops: BinOp*): BinOp = chain[BinOp, (MalExpr, MalExpr), MalExpr](ops: _*)

  def chainMalOp(ops: MalOp*): MalOp = chain[MalOp, List[MalExpr], MalExpr](ops: _*)

  def applyBinOpOrFail(op: BinOp): (MalExpr, MalExpr) => MalExpr = (arg1: MalExpr, arg2: MalExpr) => (op.lift) (arg1, arg2).getOrElse {
    throw MalEvaluationError(s"Unable to apply operation to arguments $arg1 and $arg2")
  }

  def applyMalOpOrFail(op: MalOp): List[MalExpr] => MalExpr = (args: List[MalExpr]) => (op.lift) (args).getOrElse {
    throw MalEvaluationError(s"Unable to apply operation to arguments $args")
  }

  def binOpToMFunc(initialValue: MalExpr, op: BinOp): MFunction = MFunction({
    case args => (initialValue /: args) (applyBinOpOrFail(op))
  })

  def binOpToMFunc(op: BinOp): MFunction = MFunction({
    case arg1 :: args => (arg1 /: args) (applyBinOpOrFail(op))
  })

  def malOpToMFunc(op: MalOp): MFunction = MFunction({
    case args => applyMalOpOrFail(op)(args)
  })

  lazy val add: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => MInteger(a + b)
  }

  lazy val sub: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => MInteger(a - b)
  }

  lazy val mul: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => MInteger(a * b)
  }

  lazy val div: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => MInteger(a / b)
  }

  lazy val lt: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => (a < b).mb
  }

  lazy val lte: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => (a <= b).mb
  }

  lazy val gt: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => (a > b).mb
  }

  lazy val gte: BinOp = chainBinOp {
    case (MInteger(a), MInteger(b)) => (a >= b).mb
  }

  lazy val eq: BinOp = chainBinOp {
    case (a, b) => (a == b).mb
  }

  lazy val prn: MalOp = chainMalOp {
    case (values: List[MalExpr]) =>
      println(values.map(AstPrinter.print(_)).mkString(" "))
      MNil
  }

  lazy val prnln: MalOp = chainMalOp {
    case (values: List[MalExpr]) =>
      println(values.map(AstPrinter.print(_, readable = false)).mkString(" "))
      MNil
  }

  lazy val prstr: MalOp = chainMalOp {
    case (values: List[MalExpr]) =>
      MString(values.map(AstPrinter.print(_)).mkString(" "))
  }

  lazy val str: MalOp = chainMalOp {
    case (values: List[MalExpr]) =>
      MString(values.map(AstPrinter.print(_, readable = false)).mkString(" "))
  }

  lazy val list: MalOp = chainMalOp {
    case (values: List[MalExpr]) =>
      MList(values)
  }

  lazy val islist: MalOp = chainMalOp {
    case (_: MList) :: Nil => MTrue
    case _ :: Nil => MFalse
  }

  lazy val isempty: MalOp = chainMalOp {
    case MList(elements) :: Nil if elements.isEmpty => MTrue
    case MList(_) :: Nil => MFalse
  }

  lazy val count: MalOp = chainMalOp {
    case MList(elements) :: _ => elements.length.mi
    case _ => 0.mi
  }

  lazy val not: MalOp = chainMalOp {
    case MFalse :: Nil => MTrue
    case MNil :: Nil => MTrue
    case _ => MFalse
  }

  lazy val readString: MalOp = chainMalOp {
    case MString(input) :: Nil => MalParser(input) match {
      case Right(result) => result
      case Left(ex) => throw ex
    }
  }

  lazy val slurp: MalOp = chainMalOp {
    case MString(filename) :: Nil => MString(Source.fromFile(filename).mkString)
  }

  lazy val eval: MalOp = chainMalOp {
    case ast :: Nil => Interpreter.evaluate(ast) match {
      case Right(result) => result
      case Left(ex) => throw ex
    }
  }

  lazy val loadfile: MalOp = chainMalOp {
    case args =>
      val MString(prog) = slurp(args)
      val surroundedProg = s"(do $prog)"
      eval(List(readString(List(MString(surroundedProg)))))
  }

  lazy val atom: MalOp = chainMalOp {
    case value :: Nil => Atom(value)
  }

  lazy val isatom: MalOp = chainMalOp {
    case Atom(_) :: Nil => MTrue
    case _ :: Nil => MFalse
  }

  lazy val deref: MalOp = chainMalOp {
    case Atom(value) :: Nil => value
  }

  lazy val reset: MalOp = chainMalOp {
    case (atom: Atom) :: (newValue: MalExpr) :: Nil =>
      atom.value = newValue
      newValue
  }


  lazy val swap: MalOp = chainMalOp {
    case (atom: Atom) :: (MFunction(fn)) :: args =>
      atom.value = fn(atom :: args)
      atom.value
    case (atom: Atom) :: (MClojure(fn, _, _, _)) :: args =>
      atom.value = fn(atom :: args)
      atom.value
  }
  val env: Environment = {
    val env = new Environment()

    env.define("+".msym, binOpToMFunc(MInteger(0), add))
    env.define("-".msym, binOpToMFunc(sub))
    env.define("*".msym, binOpToMFunc(MInteger(1), mul))
    env.define("/".msym, binOpToMFunc(div))
    env.define("<".msym, binOpToMFunc(lt))
    env.define("<=".msym, binOpToMFunc(lte))
    env.define(">".msym, binOpToMFunc(gt))
    env.define(">=".msym, binOpToMFunc(gte))
    env.define("=".msym, binOpToMFunc(eq))
    env.define("prn".msym, malOpToMFunc(prn))
    env.define("println".msym, malOpToMFunc(prnln))
    env.define("pr-str".msym, malOpToMFunc(prstr))
    env.define("str".msym, malOpToMFunc(str))
    env.define("list".msym, malOpToMFunc(list))
    env.define("list?".msym, malOpToMFunc(islist))
    env.define("empty?".msym, malOpToMFunc(isempty))
    env.define("count".msym, malOpToMFunc(count))
    env.define("not".msym, malOpToMFunc(not))
    env.define("read-string".msym, malOpToMFunc(readString))
    env.define("slurp".msym, malOpToMFunc(slurp))
    env.define("eval".msym, malOpToMFunc(eval))
    env.define("load-file".msym, malOpToMFunc(loadfile))

    env
  }

}
