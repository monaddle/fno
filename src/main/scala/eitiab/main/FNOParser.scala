package eitiab.main
import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}


object examples {
  val a = """val jenkinsMaster = VM(Ubuntu1404, GCP)"""
  val b = """val webServer = VM(Ubuntu1404, GCP)"""
  val c = """Application("web", Play26) deployedOn webServer storedIn Git(webServer) builtBy Jenkins(jenkinsMaster)"""
}

trait FNOPToken
case object LPAREN extends FNOPToken
case object RPAREN extends FNOPToken
case class IDENTIFIER2(value: String) extends FNOPToken
case class LITERAL2(value: String) extends FNOPToken
case object COMMA extends FNOPToken
case class ANYTHING() extends FNOPToken
case object VAL extends FNOPToken
case object EQUALS extends FNOPToken

class fnop2 extends RegexParsers {
  def lparen = "\\(".r ^^ { _ => LPAREN}
  def rparen = "\\)".r ^^ { _ => RPAREN}
  def comma = """,""".r ^^ { _ => COMMA}
  def anything = ".".r ^^ { _ => ANYTHING()}
  def literal2: Parser[LITERAL2] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL2 (content)
    }
  }
  def `val` = """val""".r ^^ { _ => VAL}
  def equals = """=""".r ^^ { _ => EQUALS}
  def identifier = """[a-zA-Z0-9]+""".r ^^ { str => IDENTIFIER2(str)}
  def tokens: Parser[List[FNOPToken]] = rep1(`val` | lparen | rparen | literal2 | identifier | comma | equals | anything)
}

class FnopReader(tokens: Seq[FNOPToken]) extends Reader[FNOPToken] {
  override def first: FNOPToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[FNOPToken] = new FnopReader(tokens.tail)

}

object FNOParser extends Parsers {
  override type Elem = FNOPToken

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case IDENTIFIER2(name) => Identifier(name) })
  }

  private def literal: Parser[Literal] = {
    accept("string literal", { case  LITERAL2(name) => Literal(name) })
  }

  private def declaration: Parser[Declaration] = {
    VAL ~ identifier ~ EQUALS ~ expression ^^ { case _ ~ id ~ _ ~ expr => Declaration(id, expr)}
  }

  private def instantiation: Parser[Instantiation] = {
    (identifier ~ LPAREN ~ expressionSequence ~ RPAREN) ^^ {case id ~ _ ~ exprs ~_ => Instantiation(id, exprs)}
  }

  private def expressionSequence: Parser[ExpressionSequence] = {
    (expression ~ rep(COMMA ~ expression).?) ^^ {case head ~ tail =>
    ExpressionSequence(List(head) ++ tail.map(_.map(_._2)).getOrElse(Nil))}
  }
  private def expression: Parser[FnopExpression] = {
    declaration | instantiation| identifier | literal
  }
  private def program: Parser[FnopAST] = {
    rep1(expression) ^^ { x => x reduceRight AndThen}
  }
  def apply(tokens: Seq[FNOPToken]): Either[String, FnopAST] = {
    val reader = new FnopReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(msg)
      case Success(result, next) => Right(result)
    }
  }
}

trait FnopExpression extends FnopAST
trait FnopAST
case class Entity(`type`: String, name: String) extends FnopAST
case class Declaration(id: Identifier, expression: FnopExpression) extends FnopAST with FnopExpression
case class Identifier(value: String) extends FnopAST with FnopExpression
case class Literal(value: String) extends FnopAST with FnopExpression
case class Instantiation(`type`: Identifier, params: ExpressionSequence) extends FnopAST with FnopExpression
case class Expression() extends FnopAST
case class ExpressionSequence(expressions: List[FnopExpression]) extends FnopAST
case class AndThen(fst: FnopAST, scd: FnopAST) extends FnopAST