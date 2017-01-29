package hu.lexan.mal.eval

import hu.lexan.mal.ast._
import hu.lexan.mal.error.{MalError, MalEvaluationError}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Interpreter {

  def tryToEither(t: Try[MalExpr]): Either[MalError, MalExpr] = t match {
    case Success(expr) => Right(expr)
    case Failure(ex: MalError) => Left(ex)
    case Failure(ex) => Left(MalEvaluationError(s"Unknown error: ${ex.getMessage}", MNil))
  }

  def evaluate(ast: MalExpr): Either[MalError, MalExpr] = {
    tryToEither(Try {
      evaluate(ast, Prelude.env)
    })
  }

  private def letEnv(defs: List[MalExpr], outer: Environment): Environment = {
    val definitions = defs.grouped(2).map {
      case (key: MSymbol) :: value :: Nil => (key, value)
      case elements => throw MalEvaluationError(s"Key '${elements.head}' is not a symbol", MList(defs))
    }.toBuffer
    val env = new Environment(Some(outer))
    definitions.foreach { case (key, value) =>
      val evaluated = evaluate(value, env)
      env.define(key, evaluated)
      env
    }
    env
  }

  private final def handleDo(ast: MList, env: Environment, rest: List[MalExpr]): MalExpr = {
    val it = rest.iterator
    var next: MalExpr = null
    while (it.hasNext) {
      next = it.next()
      if (it.hasNext)
        evaluate(next, env)
    }
    next
  }

  private final def handleIf(ast: MList, env: Environment, rest: List[MalExpr]): MalExpr = rest match {
    case first :: truthBranch :: falseBranch :: Nil => evaluate(first, env) match {
      case MFalse => falseBranch
      case MNil => falseBranch
      case _ => truthBranch
    }
    case first :: truthBranch :: Nil => evaluate(first, env) match {
      case MFalse => MNil
      case MNil => MNil
      case _ => truthBranch
    }
    case _ => throw MalEvaluationError("Invalid number of elements in if expression", ast)
  }

  def createClojure(ast: MList, env: Environment, rest: List[MalExpr]): MalExpr = rest match {
    case a1 :: a2 :: Nil =>
      val params = a1 match {
        case MList(lst) => lst
        case MVector(lst) => lst
        case _ => throw MalEvaluationError("Clojure parameter list must either be a list or a vector", ast)
      }
      MClojure(args => {
        val newEnv = Environment.bind(Some(env), params, args)
        tryToEither(Try {
          evaluate(a2, newEnv)
        })
      }, a2, env, MList(params))

    case _ => throw MalEvaluationError("Clojure construction failed", ast)
  }

  private def define(ast: MList, env: Environment, rest: List[MalExpr]): MalExpr = rest match {
    case ((sym: MSymbol) :: expr :: Nil) =>
      val eval = evaluate(expr, env)
      env.define(sym, eval)
      eval
    case _ => throw MalEvaluationError("def! requires a symbol and an expression argument", ast)
  }

  private def applyFunction(args: List[MalExpr], env: Environment): MalExpr = {
    val first = args.head
    first match {
      case MFunction(fn) => fn(args.tail) match {
        case Right(res) => res
        case Left(err) => throw err
      }
      case clojure: MClojure => clojure
      case _ => throw MalEvaluationError("First element in list is not a function", MList(args))
    }
  }

  private def evaluateAst(ast: MalExpr, env: Environment): MalExpr = ast match {
    case sym: MSymbol => env.get(sym) match {
      case Right(res) => res
      case Left(err) => throw err
    }
    case MList(elems) => MList(elems.map(evaluate(_, env)))
    case MVector(elems) => MVector(elems.map(evaluate(_, env)))
    case MMap(elems) => MMap(elems.mapValues(evaluate(_, env)))
    case _ => ast
  }

  @tailrec
  def evaluate(ast: MalExpr, env: Environment): MalExpr = ast match {
    case list@MList(elems) => elems match {
      case Nil => list
      case MSymbol("def!") :: rest => define(list, env, rest)
      case MSymbol("fn*") :: rest => createClojure(list, env, rest)
      case MSymbol("let*") :: rest => rest match {
        case (MList(definitions) :: expr :: Nil) =>
          val newEnv = letEnv(definitions, env)
          evaluate(expr, newEnv)
        case (MVector(definitions) :: expr :: Nil) =>
          val newEnv = letEnv(definitions, env)
          evaluate(expr, newEnv)
        case _ => throw MalEvaluationError("Illegal 'let*' expression", list)
      }
      case MSymbol("do") :: rest =>
        val last = handleDo(list, env, rest)
        evaluate(last, env)
      case MSymbol("if") :: rest => evaluate(handleIf(list, env, rest), env)
      case _ => {
        val evalElements = evaluateAst(list, env).asInstanceOf[MList].elements
        applyFunction(evalElements, env) match {
          case MClojure(_, clojureAst, clojureEnv, MList(params)) =>
            val newEnv = Environment.bind(Some(clojureEnv), params, evalElements.tail)
            evaluate(clojureAst, newEnv)
          case n => n
        }
      }
    }
    case _ => evaluateAst(ast, env)
  }

}
