package prestwood

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/**
  * Created by danielporter on 8/15/17.
  */
trait PrestwoodToken
case object LPAREN extends PrestwoodToken
case object RPAREN extends PrestwoodToken
case class IDENTIFIER2(value: String) extends PrestwoodToken
case class LITERAL2(value: String) extends PrestwoodToken
case object COMMA extends PrestwoodToken
case class ANYTHING() extends PrestwoodToken
case object VAL extends PrestwoodToken
case object EQUALS extends PrestwoodToken
case object CLASSKEYWORD extends PrestwoodToken
case object COLON extends PrestwoodToken
case object EXTENDS extends PrestwoodToken
case object DEF extends PrestwoodToken
case object LCURL extends PrestwoodToken
case object RCURL extends PrestwoodToken
object PrestwoodLexer extends RegexParsers {
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
  def lcurl = "{" ^^ { _ => LCURL}
  def rcurl = "}" ^^ { _ => RCURL}
  def `def` = "def" ^^ { _ => DEF}
  def `extends` = "extends" ^^ {_ => EXTENDS}
  def colon = ":" ^^ { _ => COLON}
  def `val` = """val""".r ^^ { _ => VAL}
  def equals = """=""".r ^^ { _ => EQUALS}
  def identifier = """[a-zA-Z0-9]+""".r ^^ { str => IDENTIFIER2(str)}
  def classKeyword = """class""".r ^^ { _ => CLASSKEYWORD}
  def tokens: Parser[List[PrestwoodToken]] = rep1(`val` | classKeyword |
    colon |
    `def` |
    `extends`|
    lparen |
    rparen |
    literal2 |
    identifier |
    comma |
    equals |
    lcurl |
    rcurl )

  def apply(program: String) = {
    parse(tokens, program)
  }
}

class PrestwoodReader(tokens: Seq[PrestwoodToken]) extends Reader[PrestwoodToken] {
  override def first: PrestwoodToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PrestwoodToken] = new PrestwoodReader(tokens.tail)
}