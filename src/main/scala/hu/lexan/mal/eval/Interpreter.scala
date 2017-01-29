package hu.lexan.mal.eval

import cats.instances.all._
import cats.syntax.traverse._
import hu.lexan.mal.ast._
import hu.lexan.mal.error.{MalError, MalEvaluationError}

object Interpreter {

  def evaluate(ast: MalExpr): Either[MalError, MalExpr] = {
    evaluate(ast, Prelude.env)
  }

  private def letEnv(defs: List[MalExpr], outer: Environment): Either[MalError, Environment] = {
    val newEnv = new Environment(Some(outer))

    val definitions = defs.grouped(2).map {
      case (key: MSymbol) :: value :: Nil => Right((key, value))
      case elements => Left(MalEvaluationError(s"Key '${elements.head}' is not a symbol", MList(defs)))
    }.toStream.sequenceU
    definitions.flatMap { dfs =>
      dfs.foldLeft(Right(newEnv): Either[MalError, Environment]) { case (eitherEnv, (key, value)) =>
        for {
          env <- eitherEnv
          evaluated <- evaluate(value, env)
        } yield {
          newEnv.define(key, evaluated)
          newEnv
        }
      }
    }
  }

  private final def handleLet(ast: MList, env: Environment, rest: List[MalExpr]) = rest match {
    case (MList(definitions) :: expr :: Nil) =>
      letEnv(definitions, env).flatMap { newEnv =>
        evaluate(expr, newEnv)
      }
    case (MVector(definitions) :: expr :: Nil) =>
      letEnv(definitions, env).flatMap { newEnv =>
        evaluate(expr, newEnv)
      }
    case _ => Left(MalEvaluationError("Illegal 'let*' expression", ast))
  }

  private final def handleDo(ast: MList, env: Environment, rest: List[MalExpr]): Either[MalError, MalExpr] = {
    val it = rest.iterator
    var evaluated: Either[MalError, MalExpr] = null
    while (it.hasNext) {
      evaluated = evaluate(it.next(), env)
      if (evaluated.isLeft) return evaluated
    }
    evaluated
  }

  private final def handleIf(ast: MList, env: Environment, rest: List[MalExpr]): Either[MalError, MalExpr] = rest match {
    case first :: truthBranch :: falseBranch :: Nil => evaluate(first, env).flatMap {
      case MFalse => evaluate(falseBranch, env)
      case MNil => evaluate(falseBranch, env)
      case _ => evaluate(truthBranch, env)
    }
    case first :: truthBranch :: Nil => evaluate(first, env).flatMap {
      case MFalse => Right(MNil)
      case MNil => Right(MNil)
      case _ => evaluate(truthBranch, env)
    }
    case _ => Left(MalEvaluationError("Invalid number of elements in if expression", ast))
  }

  def createClojure(ast: MList, env: Environment, rest: List[MalExpr]): scala.Either[MalError, MalExpr] = rest match {
    case a1 :: a2 :: Nil => for {
      params <- a1 match {
        case MList(lst) => Right(lst)
        case MVector(lst) => Right(lst)
        case _ => Left(MalEvaluationError("Clojure parameter list must either be a list or a vector", ast))
      }
    } yield {
      MClojure(args => {
        for {
          newEnv <- Environment.bind(Some(env), params, args)
          evaluated <- evaluate(a2, newEnv)
        } yield evaluated
      }, a2, env, MList(params))
    }
    case _ => Left(MalEvaluationError("Clojure construction failed", ast))
  }

  private def define(ast: MList, env: Environment, rest: List[MalExpr]) = {
    rest match {
      case ((sym: MSymbol) :: expr :: Nil) => for {
        eval <- evaluate(expr, env)
      } yield {
        env.define(sym, eval)
        eval
      }
      case _ => Left(MalEvaluationError("def! requires a symbol and an expression argument", ast))
    }
  }

  private final def applyAst(ast: MList, env: Environment): Either[MalError, MalExpr] = ast match {
    case list@MList(Nil) => Right(list)
    case MList(MSymbol("def!") :: rest) => define(ast, env, rest)
    case MList(MSymbol("fn*") :: rest) => createClojure(ast, env, rest)
    case MList(MSymbol("let*") :: rest) => handleLet(ast, env, rest)
    case MList(MSymbol("do") :: rest) => handleDo(ast, env, rest)
    case MList(MSymbol("if") :: rest) => handleIf(ast, env, rest)
    case list: MList => applyFunction(ast, env, list)
    case _ => Left(MalEvaluationError("Invalid list expression", ast))
  }

  private def applyFunction(ast: MList, env: Environment, list: MList) = {
    for {
      evaluated <- evaluateAst(list, env)
      result <- {
        val evalElements: List[MalExpr] = evaluated.asInstanceOf[MList].elements
        val first = evalElements.head
        first match {
          case MFunction(fn) => fn(evalElements.tail)
          case MClojure(_, clojureAst, clojureEnv, MList(params)) => for {
            newEnv <- Environment.bind(Some(clojureEnv), params, evalElements.tail)
            evaluated <- evaluate(clojureAst, newEnv)
          } yield evaluated
          case _ => Left(MalEvaluationError("First element in list is not a function", ast))
        }
      }
    } yield result
  }

  private def evaluateAst(ast: MalExpr, env: Environment): Either[MalError, MalExpr] = ast match {
    case sym: MSymbol => env.get(sym)
    case MList(elems) => elems match {
      case _ => elems.traverseU(evaluate(_, env)).map { items => MList(items) }
    }
    case MVector(elems) => elems.traverseU(evaluate(_, env)).map { items => MVector(items) }
    case MMap(elems) => elems.mapValues(evaluate(_, env)).sequenceU.map { m => MMap(m) }
    case _ => Right(ast)
  }

  def evaluate(ast: MalExpr, env: Environment): Either[MalError, MalExpr] = ast match {
    case list: MList => applyAst(list, env)
    case _ => evaluateAst(ast, env)
  }

}
