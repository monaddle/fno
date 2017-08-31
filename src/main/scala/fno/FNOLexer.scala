package fno

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/**
  * Created by danielporter on 8/15/17.
  */
trait FNOPToken
case object LPAREN extends FNOPToken
case object RPAREN extends FNOPToken
case class IDENTIFIER2(value: String) extends FNOPToken
case class LITERAL2(value: String) extends FNOPToken
case object COMMA extends FNOPToken
case class ANYTHING() extends FNOPToken
case object VAL extends FNOPToken
case object EQUALS extends FNOPToken
case object CLASSKEYWORD extends FNOPToken
case object COLON extends FNOPToken
case object EXTENDS extends FNOPToken
case object DEF extends FNOPToken
case object LCURL extends FNOPToken
case object RCURL extends FNOPToken
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
  def lcurl = "{" ^^ { _ => LCURL}
  def rcurl = "}" ^^ { _ => RCURL}
  def `def` = "def" ^^ { _ => DEF}
  def `extends` = "extends" ^^ {_ => EXTENDS}
  def colon = ":" ^^ { _ => COLON}
  def `val` = """val""".r ^^ { _ => VAL}
  def equals = """=""".r ^^ { _ => EQUALS}
  def identifier = """[a-zA-Z0-9]+""".r ^^ { str => IDENTIFIER2(str)}
  def classKeyword = """class""".r ^^ { _ => CLASSKEYWORD}
  def tokens: Parser[List[FNOPToken]] = rep1(`val` | classKeyword |
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
}

class FnopReader(tokens: Seq[FNOPToken]) extends Reader[FNOPToken] {
  override def first: FNOPToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[FNOPToken] = new FnopReader(tokens.tail)
}