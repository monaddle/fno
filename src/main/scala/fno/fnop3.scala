package fno
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

/**
  * Created by danielporter on 8/31/17.
  */

import slamdata.Predef._
import matryoshka._
import scalaz._

sealed trait PrestwoodAST[A]

object PrestwoodAST {
  case class Declaration[A](id: String, expression: A) extends PrestwoodAST[A]
  case class Block[A](contains: List[A]) extends PrestwoodAST[A]
  case class FunctionInvocation[A](id: String, args: List[A]) extends PrestwoodAST[A]
  case class FunctionDefinition[A](id: String, args: List[A]) extends PrestwoodAST[A]
  case class AndThen[A](fst: A, scd: A) extends PrestwoodAST[A]
  case class ArgumentDefinition[A](name: String, `type`: String) extends PrestwoodAST[A]
  case class Assignment[A](name: String, expr: A) extends PrestwoodAST[A]
  case class StringLiteral[A](value: String) extends PrestwoodAST[A]
  case class Instantiation[A](id: String, arg: A) extends PrestwoodAST[A]
  case class ClassDeclaration[A](id: String, params: List[A], parent: Option[String], block: A) extends PrestwoodAST[A]


  implicit val fnopast3Functor: Functor[PrestwoodAST] = new Functor[PrestwoodAST] {
    def map[A, B](ast: PrestwoodAST[A])(f: A=> B): PrestwoodAST[B] = ast match {
      case Declaration(id, expression) => Declaration(id, f(expression))
      case Block(contains) => Block(contains map f)
      case FunctionInvocation(id, args) => FunctionInvocation(id, args map f)
      case FunctionDefinition(id, args) => FunctionDefinition(id, args map f)
      case AndThen(fst, scd) => AndThen(f(fst), f(scd))
      case ArgumentDefinition(name, t) => ArgumentDefinition(name, t)
      case Assignment(name, expr) => Assignment(name, f(expr))
      case Instantiation(id, args) => Instantiation(id, f(args))
      case StringLiteral(v) => StringLiteral(v)
      case ClassDeclaration(id, params, parent, block) => ClassDeclaration[B](id, params map f, parent, f(block))
    }
  }

  def declaration(id: String, expression: Fix[PrestwoodAST]) = Fix(Declaration(id, expression))
  def block(contains: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(Block(contains))
  def functionInvocation(id: String, args: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(FunctionInvocation(id, args))
  def functionDefinition(id: String, args: List[Fix[PrestwoodAST]]): Fix[PrestwoodAST] = Fix(FunctionDefinition(id, args))
  def andThen(fst: Fix[PrestwoodAST], scd: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(AndThen(fst, scd))
  def argumentDefinition(name: String, `type`: String): Fix[PrestwoodAST] = Fix(ArgumentDefinition(name, `type`))
  def assignment(name: String, expr: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(Assignment(name, expr))
  def stringLiteral(value: String): Fix[PrestwoodAST] = Fix(StringLiteral(value))
  def instantiation(id: String, arg: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(Instantiation(id, arg))
  def classDelcaration(id: String, params: List[Fix[PrestwoodAST]], parent: Option[String], block: Fix[PrestwoodAST]): Fix[PrestwoodAST] = Fix(ClassDeclaration(id, params, parent, block))


  def collectInstantationNames: Algebra[PrestwoodAST, List[String]] = {
    case Instantiation(id, _) => List(id)
    case StringLiteral(_) => Nil
    case Assignment(_, expr) => expr
    case Block(x) => x.flatten
    case FunctionInvocation(_, args) => args.flatten
    case FunctionDefinition(id, args) => Nil
    case AndThen(fst, scd) => fst ++ scd
    case ArgumentDefinition(_, _) => Nil
    case ClassDeclaration(_, _, _, _) => Nil
    case Declaration(id, expression) => expression
  }

  def collectClassDeclarations: Algebra[PrestwoodAST, List[ClassDeclaration[_]]] = {
    case Instantiation(id, _) => Nil
    case StringLiteral(_) => Nil
    case Assignment(_, expr) => Nil
    case Block(x) => Nil
    case FunctionInvocation(_, args) => Nil
    case FunctionDefinition(id, args) => Nil
    case AndThen(fst, scd) => fst ++ scd
    case ArgumentDefinition(_, _) => Nil
    case x @ ClassDeclaration(_, _, _, _) => List(x)
    case Declaration(id, expression) => Nil
  }

  def a: List[String] = {
    println(block(List(assignment("myContainer", instantiation("DockerImage", stringLiteral("hello/world"))))).cata(collectClassDeclarations))

    block(List(assignment("myContainer", instantiation("DockerImage", stringLiteral("hello/world"))))).cata(collectInstantationNames)

  }
}
sealed trait BTree[A]

object BTree {

  case class BTNil[A]() extends BTree[A]
  case class Leaf[A](value: Int) extends BTree[A]
  case class Node[A](l: A , r: A) extends BTree[A]

  implicit val treeFunctor: Functor[BTree] = new Functor[BTree] {
    def map[A, B](tree: BTree[A])(f: A => B): BTree[B] = tree match {
      case BTNil() => BTNil()
      case Leaf(v) => Leaf(v)
      case Node(l, r) => Node(f(l), f(r))
    }
  }

  def merge(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case(l, Nil) => l
      case(Nil, r) => r
      case(lh :: ls, rh :: rs) =>
        if (lh < rh) lh::merge(ls, r)
        else rh :: merge(l, rs)
    }

  //builds a balanced binary tree
  val to: Coalgebra[BTree, List[Int]] = {
    case Nil => BTNil()
    case x :: Nil => Leaf(x)
    case xs => val (l, r) = xs.splitAt(xs.length / 2)
      Node(l, r)
  }

  //sorts a balanced binary tree
  val mergeSort: Algebra[BTree, List[Int]] = {
    case BTNil() => Nil
    case Leaf(v) => v :: Nil
    case Node(l, r) => merge(l, r)
  }
}
