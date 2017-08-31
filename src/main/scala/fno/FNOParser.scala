package fno
import cats.data.WriterT
import cats.data.Writer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
import cats.implicits._
import cats.data
import shapeless.Id
import cats.syntax.writer._

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


object FNOParser extends Parsers {
  override type Elem = FNOPToken

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case IDENTIFIER2(name) => Identifier(name) })
  }

  private def literal: Parser[Literal] = {
    accept("string literal", { case  LITERAL2(name) => Literal(name) })
  }

  private def assignment: Parser[Assignment] = {
    (identifier <~ EQUALS) ~ expression ^^ {case id ~ expression => Assignment(id, expression)}
  }

  private def declaration: Parser[Declaration] = {
    VAL ~ identifier ~ EQUALS ~ expression ^^ { case _ ~ id ~ _ ~ expr => Declaration(id, expr)}
  }

  private def instantiation: Parser[Instantiation] = {
    (identifier ~ LPAREN ~ assignmentSequence ~ RPAREN) ^^ {case id ~ _ ~ exprs ~_ => Instantiation(id, exprs)}
  }
  private def assignmentSequence: Parser[List[Assignment]] = {
    rep1(assignment <~ COMMA.?)
  }

  def functionInvocation: Parser[FunctionInvocation] = identifier ~ (LPAREN ~> repsep(expression, COMMA) <~ RPAREN) ^^ {case id ~ args => FunctionInvocation(id, args)}

  private def expression: Parser[FnopExpression] = {
    classDeclarationWithBlock | classDeclaration | declaration | functionDef | instantiation| assignment | functionInvocation| identifier | literal
  }

  def argsDefinitions: Parser[List[ArgumentDefinition]] = LPAREN ~> rep(argumentDefinition) <~ RPAREN

  private def classDeclaration: Parser[ClassDeclaration] = CLASSKEYWORD ~> identifier ~ argsDefinitions ~ (EXTENDS ~> identifier).? ^^ {
    case id ~ attrs ~ parent => ClassDeclaration(id,attrs, parent, Block(Nil))
  }

  private def classDeclarationWithBlock: Parser[ClassDeclaration] = classDeclaration ~ block ^^ {case declaration ~ block => declaration.copy(block=block)}

  def block: Parser[Block] = LCURL ~> rep(expression) <~ RCURL ^^ { x => Block(x)}

  def functionDef: Parser[FunctionDefinition] = (DEF ~> identifier) ~ argsDefinitions ~ block ^^ {case id ~ argsDefs ~ block => FunctionDefinition(id, argsDefs, block)}//{ case id~ args ~ block => FunctionDefinition(id, block)}

  private def argumentDefinition: Parser[ArgumentDefinition] = (identifier <~ COLON) ~ identifier <~ COMMA.? ^^ {case id ~ t => ArgumentDefinition(id, t) }

  private def program: Parser[FnopAST] = {
    rep1(expression) ^^ { x => x reduceRight AndThen}
  }

  def apply(tokens: Seq[FNOPToken]): Either[String, FnopAST] = {
    val reader = new FnopReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(msg)
      case Success(result, next) =>
        Right(result)
    }
  }
}





