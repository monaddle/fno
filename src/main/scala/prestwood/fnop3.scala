package prestwood

import matryoshka.data._

/**
  * Created by danielporter on 8/31/17.
  */

import matryoshka._
import slamdata.Predef._

import scalaz._

import slamdata.Predef._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._

sealed trait PrestwoodAST[A]

object PrestwoodAST {
  case class Declaration[A](id: A, expression: A) extends PrestwoodAST[A]
  case class Block[A](contains: List[A]) extends PrestwoodAST[A]
  case class FunctionInvocation[A](id: A, args: List[A]) extends PrestwoodAST[A]
  case class FunctionDefinition[A](id: A, args: List[A], block: A) extends PrestwoodAST[A]
  case class AndThen[A](fst: A, scd: A) extends PrestwoodAST[A]
  case class ArgumentDefinition[A](id: A, `type`: A) extends PrestwoodAST[A]
  case class Assignment[A](id: A, expr: A) extends PrestwoodAST[A]
  case class StringLiteral[A](value: String) extends PrestwoodAST[A]
  case class Instantiation[A](id: A, args: List[A]) extends PrestwoodAST[A]
  case class ClassDeclaration[A](id: A, params: List[A], parent: Option[A], block: A) extends PrestwoodAST[A]
  case class Identifier[A](id: String) extends PrestwoodAST[A]

  implicit val fnopast3Functor: Functor[PrestwoodAST] = new Functor[PrestwoodAST] {
    def map[A, B](ast: PrestwoodAST[A])(f: A=> B): PrestwoodAST[B] = ast match {
      case Declaration(id, expression) => Declaration(f(id), f(expression))
      case Block(contains) => Block(contains map f)
      case FunctionInvocation(id, args) => FunctionInvocation(f(id), args map f)
      case FunctionDefinition(id, args, block) => FunctionDefinition(f(id), args map f, f(block))
      case AndThen(fst, scd) => AndThen(f(fst), f(scd))
      case ArgumentDefinition(name, t) => ArgumentDefinition(f(name), f(t))
      case Assignment(name, expr) => Assignment(f(name), f(expr))
      case Instantiation(id, args) => Instantiation(f(id), args map f)
      case StringLiteral(v) => StringLiteral(v)
      case ClassDeclaration(id, params, parent, block) => ClassDeclaration[B](f(id), params map f, parent map f, f(block))
      case Identifier(id) => Identifier(id)
    }
  }

  def declaration(id: Fix[PrestwoodAST], expression: Fix[PrestwoodAST]) = Fix(Declaration(id, expression))
  def block(contains: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(Block(contains))
  def functionInvocation(id: Fix[PrestwoodAST] , args: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(FunctionInvocation(id, args))
  def functionDefinition(id: Fix[PrestwoodAST] , args: List[Fix[PrestwoodAST]], block: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(FunctionDefinition(id, args, block))
  def andThen(fst: Fix[PrestwoodAST], scd: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(AndThen(fst, scd))
  def argumentDefinition(name: Fix[PrestwoodAST] , `type`: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(ArgumentDefinition(name, `type`))
  def assignment(name: Fix[PrestwoodAST], expr: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(Assignment(name, expr))
  def assignment(name: String, expr: Fix[PrestwoodAST]): Fix[PrestwoodAST] = assignment(identifier(name), expr)
  def stringLiteral(value: String): Fix[PrestwoodAST] = Fix(StringLiteral(value))
  def instantiation(id: Fix[PrestwoodAST], arg: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(Instantiation(id, arg))
  def instantiation(id: String, arg: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = instantiation(identifier(id), arg)
  def classDelcaration(id: Fix[PrestwoodAST] , params: List[Fix[PrestwoodAST]], parent: Option[Fix[PrestwoodAST]], block: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(ClassDeclaration(id, params, parent, block))
  def identifier(id: String): Fix[PrestwoodAST] = Fix(Identifier(id))

  def collectInstantationNames: Algebra[PrestwoodAST, List[String]] = {
    case Instantiation(id, _) => id
    case StringLiteral(_) => Nil
    case Assignment(_, expr) => expr
    case Block(x) => x.flatten
    case FunctionInvocation(_, args) => args.flatten
    case FunctionDefinition(id, args, _) => Nil
    case AndThen(fst, scd) => fst ++ scd
    case ArgumentDefinition(_, _) => Nil
    case ClassDeclaration(_, _, _, _) => Nil
    case Declaration(id, expression) => expression
    case Identifier(id) => List(id)
  }

  def collectClassDeclarations: Algebra[PrestwoodAST, List[ClassDeclaration[_]]] = {
    case Instantiation(id, _) => Nil
    case StringLiteral(_) => Nil
    case Assignment(_, expr) => Nil
    case Block(x) => Nil
    case FunctionInvocation(_, args) => Nil
    case FunctionDefinition(id, args, _) => Nil
    case AndThen(fst, scd) => fst ++ scd
    case ArgumentDefinition(_, _) => Nil
    case x @ ClassDeclaration(_, _, _, _) => List(x)
    case Declaration(id, expression) => Nil
    case Identifier(name) => Nil
  }
}
