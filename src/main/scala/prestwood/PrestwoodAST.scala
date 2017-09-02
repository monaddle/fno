package prestwood


/**
  * Created by danielporter on 8/31/17.
  */


import matryoshka.data._
import scalaz._
import slamdata.Predef._
import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns._

import Scalaz._

sealed trait PrestwoodAST[A]

object PrestwoodAST {
  type FAST = Fix[PrestwoodAST]

  case class Declaration[A](id: Id[A], expression: A) extends PrestwoodAST[A]
  case class Block[A](contains: List[A]) extends PrestwoodAST[A]
  case class FunctionInvocation[A](id: Id[A], args: List[A]) extends PrestwoodAST[A]
  case class FunctionDefinition[A](id: Id[A], args: List[A], block: A) extends PrestwoodAST[A]
  case class AndThen[A](fst: A, scd: A) extends PrestwoodAST[A]
  case class ArgumentDefinition[A](id: Id[A], `type`: Id[A]) extends PrestwoodAST[A]
  case class Assignment[A](id: Id[A], expr: A) extends PrestwoodAST[A]
  case class StringLiteral[A](value: String) extends PrestwoodAST[A]
  case class Instantiation[A](id: Id[A], args: List[A]) extends PrestwoodAST[A]
  case class ClassDeclaration[A](id: Id[A], params: List[A], parent: Option[Id[A]], block: A) extends PrestwoodAST[A]
  case class Id[A](id: String) extends PrestwoodAST[A]

  implicit val fnopast3Functor: Functor[PrestwoodAST] = new Functor[PrestwoodAST] {
    def map[A, B](ast: PrestwoodAST[A])(f: A=> B): PrestwoodAST[B] = ast match {
      case Declaration(Id(id), expression) => Declaration(Id(id), f(expression))
      case Block(contains) => Block(contains map f)
      case FunctionInvocation(Id(id), args) => FunctionInvocation(Id(id), args map f)
      case FunctionDefinition(Id(id), args, block) => FunctionDefinition(Id(id), args map f, f(block))
      case AndThen(fst, scd) => AndThen(f(fst), f(scd))
      case ArgumentDefinition(Id(id), Id(t)) => ArgumentDefinition(Id(id), Id(t))
      case Assignment(Id(id), expr) => Assignment(Id(id), f(expr))
      case Instantiation(Id(id), args) => Instantiation(Id(id), args map f)
      case StringLiteral(v) => StringLiteral(v)
      case ClassDeclaration(Id(id), params, parent: Option[Id[FAST]], block) => ClassDeclaration[B](Id(id), params map f, parent map { case Id(id) => Id(id)}, f(block))
      case Id(id) => Id(id)
    }
  }
  implicit val traverse: Traverse[PrestwoodAST] = new Traverse[PrestwoodAST] {
    def traverseImpl[G[_], A, B](fa: PrestwoodAST[A])(f: A => G[B])(
      implicit G: Applicative[G]):
    G[PrestwoodAST[B]] = fa match {
      case Declaration(Id(id), b) => f(b).map(Declaration(Id(id), _))
      case Block(contains) => contains.traverse(f) ∘ (Block(_))
      case FunctionInvocation(Id(id), args) => args.traverse(f) ∘( FunctionInvocation(Id(id), _))
      case FunctionDefinition(Id(id), args, block) => (args.traverse(f) ⊛ f(block))(FunctionDefinition(Id(id), _, _))
      case AndThen(fst, scd) => (f(fst) |@| f(scd))(AndThen(_, _))
      case ArgumentDefinition(Id(id), Id(t)) => G.point(ArgumentDefinition(Id(id), Id(t)))
      case Assignment(Id(id), expr) => f(expr).map(Assignment(Id(id), _))
      case Instantiation(Id(id), args) => args.traverse(f) ∘ (Instantiation(Id(id), _))
      case StringLiteral(v) => G.point(StringLiteral(v))
      case ClassDeclaration(Id(id), params, parent: Option[Id[A]], block) => (params.traverse(f) |@| f(block)){case (x, y) =>ClassDeclaration[B](Id(id), x, parent.map{case Id(id) => Id[B](id)}, y)}
      case Id(id) => G.point(Id(id))
    }
  }
  def declaration(id: Id[FAST], expression: FAST) = Fix(Declaration(id, expression))
  def block(contains: List[FAST]): FAST = Fix(Block(contains))
  def functionInvocation(id: Id[FAST], args: List[FAST]): FAST = Fix(FunctionInvocation(id, args))
  def functionDefinition(id: Id[FAST], args: List[FAST], block: FAST): FAST = Fix(FunctionDefinition(id, args, block))
  def andThen(fst: FAST, scd: FAST): FAST = Fix(AndThen(fst, scd))
  def argumentDefinition(name: Id[FAST], `type`: Id[FAST]): FAST = Fix(ArgumentDefinition(name, `type`))
  def assignment(name: Id[FAST], expr: FAST): FAST = Fix(Assignment(name, expr))
  def assignment(name: String, expr: FAST): FAST = assignment(identifier(name), expr)
  def stringLiteral(value: String): FAST = Fix(StringLiteral(value))
  def instantiation(id: Id[FAST], arg: List[FAST]): FAST = Fix(Instantiation(id, arg))
  def instantiation(id: String, arg: List[FAST]): FAST = instantiation(identifier(id), arg)
  def classDelcaration(id: Id[FAST] , params: List[FAST], parent: Option[Id[FAST]], block: FAST): FAST = {
    Fix(ClassDeclaration(id, params, parent, block))
  }
  def identifier(id: String): Id[FAST] = Id[FAST](id)

  def collectInstantationNames: Algebra[PrestwoodAST, List[String]] = {
    case Instantiation(id, _) => List(id.id)
    case StringLiteral(_) => Nil
    case Assignment(_, expr) => expr
    case Block(x) => x.flatten
    case FunctionInvocation(_, args) => args.flatten
    case FunctionDefinition(id, args, _) => Nil
    case AndThen(fst, scd) => fst ++ scd
    case ArgumentDefinition(_, _) => Nil
    case ClassDeclaration(_, _, _, _) => Nil
    case Declaration(id, expression) => expression
    case Id(id) => List(id)
  }

  // 1. lex
  // 2. parse into AST
  // 3. collect class declarations
  //    - can only occur in outer-most scope
  //    - how to identify this? does "find all class declarations within a block" suffice?
  // 4.


  type Logged[A] = Writer[Vector[String],A]

  def CollectClassDeclarationNames: AlgebraM[Logged, PrestwoodAST, PrestwoodAST[_]] = {
    case a @ ClassDeclaration(id, x, y, z) => Writer(Vector(id.id), a)
    case x => Writer(Vector.empty[String], x)
  }
  type Written[A] = Writer[Vector[PrestwoodAST[_]],A]

  def collectClassDeclarations: AlgebraM[Written, PrestwoodAST, PrestwoodAST[_]] = {
    case a @ ClassDeclaration(id, x, y, z) => Writer(Vector(a), a)
    case x => Writer(Vector(), x)
  }
  type Para[x] = (Fix[PrestwoodAST], x)
  def b: GAlgebra[Para, PrestwoodAST, PrestwoodAST[_]] = {
    case Assignment(Id(id), expr) => Assignment(Id(id), expr._2)
    case x => x
  }
  //type CF[A] = Cofree[PrestwoodAST[_], A]
/*  def c: GAlgebra[CF, PrestwoodAST, PrestwoodAST[_]] = {
    case Assignment(Id(id), expr) => expr.extend { x=>}
  }

  def d: GAlgebraM[CF, Logged, PrestwoodAST, List[String]] = {
    case Assignment(Id(id), expr) =>
  }*/


  def e: GAlgebraM[Logged, Logged, PrestwoodAST, PrestwoodAST[_]] = {
    case Assignment(Id(id), expr) => for {
      a <- expr
      _ <- Vector(id).tell
    } yield a
    case x => Writer(Vector(), x)
  }

  def a(ast: FAST) = {
    ast.cataM(collectClassDeclarations)
    ast.para(b)
  }
}
