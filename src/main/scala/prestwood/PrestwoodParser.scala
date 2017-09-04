package prestwood

/**
  * Created by danielporter on 9/1/17.
  */
import cats.data.WriterT
import cats.data.Writer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
import cats.implicits._
import cats.data
import shapeless.Id
import cats.syntax.writer._
import matryoshka.data.Fix
import prestwood.PrestwoodAST.StringLiteral

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try

object examples {
  val a = """val jenkinsMaster = VM(Ubuntu1404, GCP)"""
  val b = """val webServer = VM(Ubuntu1404, GCP)"""
  val c = """Application("web", Play26) deployedOn webServer storedIn Git(webServer) builtBy Jenkins(jenkinsMaster)"""
  val d = """DockerImage(imageName="library/hello-world", runName="hello") deployedOn VM(Ubuntu1404, DigitalOcean) """
  val e = """def DockerImage(imageName: String, runName:String)"""
}
import prestwood._

object PrestwoodParser extends Parsers {
  override type Elem = PrestwoodToken
  type AST = Fix[PrestwoodAST]
  private def identifier: Parser[PrestwoodAST.Id[AST]] = {
    accept("identifier", { case IDENTIFIER(name) => PrestwoodAST.ID(name) })
  }

  private def literal: Parser[AST] = {
    accept("string literal", { case  LITERAL(name) => PrestwoodAST.stringLiteral(name)})
  }

  def assignment: Parser[AST] = {
    (identifier <~ EQUALS) ~ assignmentExpression ^^ {case id ~ expression => PrestwoodAST.assignment(id, expression)}
  }

  def assignmentExpression: Parser[AST] = {
    instantiation  | literal | (identifier ^^ {x=> Fix(x)})
  }

  private def declaration: Parser[AST] = {
    VAL ~ identifier ~ EQUALS ~ expression ^^ { case _ ~ id ~ _ ~ expr => PrestwoodAST.declaration(id, expr)}
  }

  private def instantiation: Parser[AST] = {
    (identifier ~ LPAREN ~ assignmentSequence ~ RPAREN) ^^ {case id ~ _ ~ exprs ~_ => PrestwoodAST.instantiation(id, exprs)}
  }
  private def assignmentSequence: Parser[List[AST]] = {
    rep1(assignment <~ COMMA.?)
  }

  def functionInvocation: Parser[AST] = identifier ~ (LPAREN ~> repsep(expression, COMMA) <~ RPAREN) ^^ {case id ~ args => PrestwoodAST.functionInvocation(id, args)}

  private def expression: Parser[AST] = {
    classDeclaration | declaration | functionDef | instantiation| assignment | functionInvocation | literal
  }

  def argsDefinitions: Parser[List[PrestwoodAST.ArgumentDefinition[AST]]] = LPAREN ~> rep(argumentDefinition) <~ RPAREN

  private def classDeclaration: Parser[AST] = CLASSKEYWORD ~> identifier ~ argsDefinitions ~ ((EXTENDS ~> identifier).?) ~ (block.?) ^^ {
    case id ~ attrs ~ parent ~ block => PrestwoodAST.classDeclaration(id,attrs, parent, block.getOrElse(PrestwoodAST.Block(Nil)))
  }

  //private def classDeclarationWithBlock: Parser[AST] = classDeclaration ~ block ^^ {case declaration ~ block => declaration.copy(block=block)}

  def block: Parser[PrestwoodAST.Block[AST]] = LCURL ~> rep(expression) <~ RCURL ^^ { x => PrestwoodAST.Block(x)}

  def functionDef: Parser[AST] = (DEF ~> identifier) ~ argsDefinitions ~ block ^^ {case id ~ argsDefs ~ block => PrestwoodAST.functionDefinition(id, argsDefs, block)}//{ case id~ args ~ block => FunctionDefinition(id, block)}

  private def argumentDefinition: Parser[PrestwoodAST.ArgumentDefinition[AST]] = (identifier <~ COLON) ~ identifier <~ COMMA.? ^^ {case id ~ t => PrestwoodAST.ArgumentDefinition(id, t) }

  private def program: Parser[AST] = {
    rep1(expression) ^^ { x => x reduceRight PrestwoodAST.andThen}
  }

  def apply(tokens: Seq[PrestwoodToken]): Either[String, AST] = {
    val reader = new PrestwoodReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(msg)
      case Success(result, next) =>
        println(next.atEnd)
        Right(result)
    }
  }
}





