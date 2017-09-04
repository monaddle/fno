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
import org.scalacheck.Cogen
import org.scalacheck._
import scalaz._, Scalaz._

import Scalaz._

sealed trait PrestwoodAST[A] {
  import PrestwoodAST.fnopast3Functor
  def map[_, B](f: A => B): PrestwoodAST[B] = fnopast3Functor(this)(f)
}

object PrestwoodAST {
  type FAST = Fix[PrestwoodAST]
  def parse[A](toParse: String): Either[String, FAST] = {
    PrestwoodParser(PrestwoodLexer(toParse).get)
  }

  implicit val cogen: Delay[Cogen, PrestwoodAST] = new Delay[Cogen, PrestwoodAST] {
    def apply[A](co: Cogen[A]) = Cogen((seed, value) => seed)
  }

  case class Declaration[A](id: Id[_], expression: A) extends PrestwoodAST[A]
  case class Block[A](contains: List[A]) extends PrestwoodAST[A]
  case class FunctionInvocation[A](id: Id[_], args: List[A]) extends PrestwoodAST[A]
  case class FunctionDefinition[A](id: Id[_], args: List[ArgumentDefinition[_]], block: Block[A]) extends PrestwoodAST[A]
  case class AndThen[A](fst: A, scd: A) extends PrestwoodAST[A]
  case class ArgumentDefinition[A](id: Id[_], `type`: Id[_]) extends PrestwoodAST[A]
  case class Assignment[A](id: Id[_], expr: A) extends PrestwoodAST[A]
  case class StringLiteral[A](value: String) extends PrestwoodAST[A]
  case class Instantiation[A](id: Id[_], args: List[A]) extends PrestwoodAST[A]
  case class ClassDeclaration[A](id: Id[_], params: List[ArgumentDefinition[_]], parent: Option[Id[_]], block: Block[A]) extends PrestwoodAST[A]
  case class Id[A](id: String) extends PrestwoodAST[A]

  implicit val fnopast3Functor: Functor[PrestwoodAST] = new Functor[PrestwoodAST] {
    def map[A, B](ast: PrestwoodAST[A])(f: A=> B): PrestwoodAST[B] = ast match {
      case Declaration(Id(id), expression) => Declaration(Id(id), f(expression))
      case Block(contains) => Block(contains map f)
      case FunctionInvocation(Id(id), args) => FunctionInvocation(Id(id), args map f)
      case FunctionDefinition(Id(id), args, Block(b)) => FunctionDefinition(Id(id), args, Block(b map f))
      case AndThen(fst, scd) => AndThen(f(fst), f(scd))
      case ArgumentDefinition(Id(id), Id(t)) => ArgumentDefinition(Id(id), Id(t))
      case Assignment(Id(id), expr) => Assignment(Id(id), f(expr))
      case Instantiation(Id(id), args) => Instantiation(Id(id), args map f)
      case StringLiteral(v) => StringLiteral(v)
      case ClassDeclaration(Id(id), params, parent: Option[Id[FAST]], Block(b)) => ClassDeclaration[B](Id(id), params, parent map { case Id(id) => Id(id)},Block(b.map(f)))
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
      case FunctionDefinition(Id(id), args, Block(b)) => (b traverse f).map(x => FunctionDefinition(Id(id), args, Block(x)))
      case AndThen(fst, scd) => (f(fst) |@| f(scd))(AndThen(_, _))
      case ArgumentDefinition(Id(id), Id(t)) => G.point(ArgumentDefinition(Id(id), Id(t)))
      case Assignment(Id(id), expr) => f(expr).map(Assignment(Id(id), _))
      case Instantiation(Id(id), args) => args.traverse(f) ∘ (Instantiation(Id(id), _))
      case StringLiteral(v) => G.point(StringLiteral(v))
      case ClassDeclaration(Id(id), params, parent, Block(b)) => (b traverse f).map( x=> ClassDeclaration[B](Id(id), params, parent, Block(x))      )
      case Id(id) => G.point(Id(id))
    }
  }
  def declaration(id: Id[FAST], expression: FAST) = Fix(Declaration(id, expression))
  def block(contains: List[FAST]): Fix[PrestwoodAST] = Fix(Block(contains))
  def functionInvocation(id: Id[FAST], args: List[FAST]): FAST = Fix(FunctionInvocation(id, args))
  def functionDefinition(id: Id[FAST], args: List[ArgumentDefinition[FAST]], block: Block[FAST]): FAST = {
    Fix(FunctionDefinition[FAST](id, args, block)     )
  }
  def andThen(fst: FAST, scd: FAST): FAST = Fix(AndThen(fst, scd))
  def argumentDefinition(name: Id[FAST], `type`: Id[FAST]) = Fix(ArgumentDefinition[FAST](name, `type`))
  def assignment(name: Id[FAST], expr: FAST): FAST = Fix(Assignment(name, expr))
  def assignment(name: String, expr: FAST): FAST = assignment(ID(name), expr)
  def stringLiteral(value: String): FAST = Fix(StringLiteral(value))
  def instantiation(id: Id[FAST], arg: List[FAST]): FAST = Fix(Instantiation(id, arg))
  def instantiation(id: String, arg: List[FAST]): FAST = instantiation(ID(id), arg)
  def classDeclaration(id: Id[FAST], params: List[ArgumentDefinition[_]], parent: Option[Id[FAST]], block: Block[FAST]): FAST = {
    Fix(ClassDeclaration(id, params, parent, block))
  }
  def ID(id: String): Id[FAST] = Id[FAST](id)

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


  type Written[A] = Writer[Vector[ClassDeclaration2],A]

  case class ClassDeclaration2(name: String, args: List[(String, String)], block: Block[PrestwoodAST[_]])

  def collectClassDeclarations: AlgebraM[Written, PrestwoodAST, PrestwoodAST[_]] = {
    case a @ ClassDeclaration(id, argdefs, y, z) =>
      Writer(Vector(ClassDeclaration2(id.id, argdefs map { x=> (x.id.id,  x.`type`.id)}, z)), a)
    case x => Writer(Vector(), x)
  }

  def collectClassDeclarations(ast: FAST): Written[PrestwoodAST[_]] = ast.cataM(collectClassDeclarations)
  type Para[x] = (Fix[PrestwoodAST], x)
  def b: GAlgebra[Para, PrestwoodAST, PrestwoodAST[_]] = {
    case Assignment(Id(id), expr) => Assignment(Id(id), expr._2)
    case x => x
  }

  def collectcd(ast: FAST) = ast.cata(collectCD)

  def collectCD: Algebra[PrestwoodAST, List[PrestwoodAST[_]]] = {
    case a @ClassDeclaration(_, _, _, _) => List(a)
    case x =>  children(x).flatten
  }




  def e: GAlgebraM[Logged, Logged, PrestwoodAST, PrestwoodAST[_]] = {
    case Assignment(Id(id), expr) => for {
      a <- expr
      _ <- Vector(id).tell
    } yield a
    case x => Writer(Vector(), x)
  }

  def replaceInPlace: (Fix[PrestwoodAST] => Fix[PrestwoodAST]) = {
    case Fix(Assignment(Id(id), expr)) => Fix(Assignment(Id("works??"), expr))
    case x => x
  }

  def replaceinplace(x: FAST) = x.transCataT(replaceInPlace)

  def checkstuff: (Fix[PrestwoodAST]) => Logged[Fix[PrestwoodAST]] = {
    case a @ Fix(AndThen(fst, scd)) => println("and then!")
      Writer(Vector("andthen"), a)
    case x=> Writer(Vector(), x)
  }
  type cf[A] = Cofree[PrestwoodAST, A]




  def children[A]: (PrestwoodAST[A] => List[A]) = {
    case ArgumentDefinition(Id(id), t) => Nil
    case AndThen(fst, scd) => List(fst, scd)
    case Assignment(id, expr) => List(expr)
    case Block(x) => x
    case ClassDeclaration(id, x, y, Block(b)) => b ++ x.asInstanceOf[List[A]]
    case Declaration(id, expr) => List(expr)
    case FunctionDefinition(id, args, block) => block.contains
    case FunctionInvocation(id, args) => args
    case Instantiation(id, args) => args
    case Id(id) => Nil
    case StringLiteral(value) => Nil
  }

  type Annotation[A] = (String, A)
  def checkForDoubled: Coalgebra[List, PrestwoodAST[_]] = {
    case x => List(x)
  }


  def tt: (FAST => Scalaz.Id[FAST]) = {
    case x => x
  }






  type envt[A] = EnvT[Int, PrestwoodAST, A]


  def maybe2: PrestwoodAST[Fix[PrestwoodAST]] => EnvT[Int, PrestwoodAST, FAST] = {
    case b @ Block(a) => EnvT(a.length, b)
    case at @ AndThen(_, _) => EnvT(2, at)
    case x => EnvT(1, x)
  }

  def annotateWithChildrenCount: PrestwoodAST[Cofree[PrestwoodAST, Int]] => EnvT[Int, PrestwoodAST, Cofree[PrestwoodAST, Int]] = {
    case x => EnvT(1 + (children(x) map { x=> x.head} sum), x)
  }

  def plzwork2(ast: FAST): Cofree[PrestwoodAST, Int] =  ast.transCata[Cofree[PrestwoodAST, Int]][envt](annotateWithChildrenCount)


  def plzwork(ast: FAST): Cofree[PrestwoodAST, Int] =  ast.transAna[Cofree[PrestwoodAST, Int]][envt](maybe2)






  def printCofree: Cofree[PrestwoodAST, Cofree[PrestwoodAST, Int]] => Cofree[PrestwoodAST, Cofree[PrestwoodAST, Int]] = {
    x => x.foldMap{ cf => println(cf.tail)
    List(cf)}
      x
  }

  def printCofreeSimple: Cofree[PrestwoodAST, Int] => Cofree[PrestwoodAST, Int] = {
    x =>
      x.applyCofree(x=> 3, cf => cf)
      println(x.tail)
      x
  }

  def printcofree(x: Cofree[PrestwoodAST, Cofree[PrestwoodAST, Int]]) = x.transCataT(printCofree)
  def printcofreesimple(x: Cofree[PrestwoodAST, Int]) = x.transCataT(printCofreeSimple)









  case class SymbolTable(id: String)








  /*

      def ga2: GAlgebra[tpl, PrestwoodAST, List[String]] = {
        case Declaration(x, y) => List(x.id)
        case Block(x) => x flatMap (_._2)
        case AndThen(fst, scd) => fst._2 ++ scd._2
        case x=> Nil
      }
    */


  //def x(ast: FAST) = ast.para(ga2)









  type MAP[A] = Map[PrestwoodAST[Fix[PrestwoodAST]], A]

  case class DeclarationsInBlock(seq: Id[_])

  type LoggedDeclaration[A] = Writer[Vector[DeclarationsInBlock], A]

  def h: GAlgebraM[Logged, Logged, PrestwoodAST, PrestwoodAST[_]] = {
    case a @Assignment(Id(id), expr) => Writer(Vector(), a)
    case x  => Writer(Vector(), x)
  }


  def f: GAlgebra[cf, PrestwoodAST, List[String]] = {
    case ClassDeclaration(id, params, parent, block) =>
      List(id.id)
    case AndThen(fst, scd) => println("fst head,tail", fst.head, fst.tail  )
      fst.head ++ scd.head
    case x =>
      val childs = children(x)
      x map { y => y}

      val childs2 = childs.flatMap(x=> x.copure)
      List(x.getClass.toString) ++ childs2
  }

  def testalgebra: AlgebraM[Logged, PrestwoodAST, PrestwoodAST[_]] = {
    case a => Writer(Vector(a.toString), a)
  }

  type l[A] = List[PrestwoodAST[A]]



  def extractSomething(id: String): PrestwoodAST[Free[PrestwoodAST, String]] = {
    if(id == "DockerContainer") Instantiation(Id(id), List.empty[Free[PrestwoodAST, String]])
    else Id(id)
  }


  def a(ast: FAST) = {
    ast.histo(f)
  }

}
