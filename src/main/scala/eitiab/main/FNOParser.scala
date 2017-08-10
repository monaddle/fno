package eitiab.main
import cats.data.WriterT
import cats.data.Writer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
import cats.implicits._
import cats.data
import shapeless.Id
import cats.syntax.writer._
trait Application
case class DockerImage(imageName: String, runName: Option[String], args: Map[String, String]) extends Application
trait OS
case class Ubuntu1404() extends OS
trait Provider
case class GCP() extends Provider
case class DigitalOcean() extends Provider
case class VM(os: OS, provider: String)

case class FnopPrimitive(attrs: Map[String, String])

object examples {
  val a = """val jenkinsMaster = VM(Ubuntu1404, GCP)"""
  val b = """val webServer = VM(Ubuntu1404, GCP)"""
  val c = """Application("web", Play26) deployedOn webServer storedIn Git(webServer) builtBy Jenkins(jenkinsMaster)"""
  val d = """DockerImage(imageName="library/hello-world", runName="hello") deployedOn VM(Ubuntu1404, DigitalOcean) """
  val e = """def DockerImage(imageName: String, runName:String)"""
}

trait FNOPToken
case object LPAREN extends FNOPToken
case object RPAREN extends FNOPToken
case class IDENTIFIER2(value: String) extends FNOPToken
case class LITERAL2(value: String) extends FNOPToken
case object COMMA extends FNOPToken
case class ANYTHING() extends FNOPToken
case object VAL extends FNOPToken
case class DEF() extends FNOPToken with Positional
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

  private def expressionSequence: Parser[ExpressionSequence] = {
    (expression ~ rep(COMMA ~ expression).?) ^^ {case head ~ tail =>
    ExpressionSequence(List(head) ++ tail.map(_.map(_._2)).getOrElse(Nil))}
  }
  private def expression: Parser[FnopExpression] = {
    declaration | instantiation| assignment | identifier | literal
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

trait Type {
  val name: String
}
case object StringValue extends Type {
  override val name: String = "string"
}
trait FnopExpression extends FnopAST
object FnopAST {
  val symbolTable = collection.mutable.HashMap.empty[String, String] += ("Ubuntu1404" -> "OS", "DigitalOcean" -> "Provider")
  val classTable =
    collection.mutable.HashMap.empty[String, Map[Identifier, String]] +=
    "DockerImage" -> Map(Identifier("imageName") -> "String", Identifier("runName") -> "String", Identifier("provider") -> "Provider", Identifier("os") -> "OS")
}
sealed trait Tree[+A]
final case class Node[+A](value: A, children: Seq[Tree[A]])
trait FnopAST {
  type Logged[A] = WriterT[Id, Vector[String], A]
  def checkTypes:  Logged[TypedFnopAST]= {
    this match {
      case Identifier(id) => val res = FnopAST.symbolTable.getOrElse(id, "any")
        Writer(Vector.empty[String], TypedIdentifier(id, res)).tell(if(res=="any") Vector(s"$id is not a valid identifier.") else Vector()).asInstanceOf[Writer[Vector[String], TypedFnopAST]]
      case Instantiation(id, assignments) =>
        val writert = WriterT[List, Vector[String], Assignment](assignments map ((Vector.empty[String], _)))
        val a = writert map { m =>
          m.checkTypes
        }


        val typedAssignmentsW = (assignments map { assignment =>
          for {
            x <- assignment.checkTypes
          } yield x.asInstanceOf[TypedAssignment]
        }).foldRight(Writer[Vector[String], Vector[TypedAssignment]](Vector(),Vector())) { case (c, b) =>
             b.mapBoth{(log, assignments) =>
               val cRun = c.run
               (log ++ cRun._1, assignments ++ Vector(cRun._2))
             }
         }


        val res =typedAssignmentsW flatMap { assignments =>
          val typedInstantiation = TypedInstantiation(id, assignments.toList, id.value)
          FnopAST.classTable.get(id.value) map { params =>
            val a = assignments flatMap { assignment =>
              params.get(assignment.identifier) map { paramType =>
                if (assignment.t == paramType) {
                  Vector.empty[String]
                }
                else {
                  Vector(s"$id requires type $paramType, but actual type is ${assignment.t}")
                }
              } getOrElse Vector(s"${assignment.identifier} is not a valid argument.")
            }
            Writer(a, typedInstantiation)
          } getOrElse Writer(Vector(s"$id is not a valid identifier."), typedInstantiation)
        }
        res.asInstanceOf[Writer[Vector[String], TypedFnopAST]]

      case Literal(value) => Writer(Vector.empty[String],TypedLiteral(value, "String"))
      case Assignment(id, expression) =>
        for {
          a <- expression.checkTypes

        } yield { TypedAssignment(id, a, a.t)}
    }
  }
}

trait TypedFnopAST {
  val t: String


}
trait TypedFnopExpression {
  val t: String
}

case class TypedDeclaration(id: TypedDeclaration, expression: TypedFnopExpression, t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedIdentifier(value: String, t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedLiteral(value: String, t: String) extends TypedFnopAST with TypedFnopExpression

case class TypedInstantiation(identifier: Identifier, params: List[TypedAssignment], t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedAssignment(identifier: Identifier, expression: TypedFnopAST, t: String) extends TypedFnopAST with TypedFnopExpression



case class Declaration(id: Identifier, expression: FnopExpression) extends FnopAST with FnopExpression
case class Identifier(value: String) extends FnopAST with FnopExpression
case class Literal(value: String) extends FnopAST with FnopExpression

case class Instantiation(`type`: Identifier, params: List[Assignment]) extends FnopAST with FnopExpression {
}
case class ExpressionSequence(expressions: List[FnopExpression]) extends FnopAST
case class AndThen(fst: FnopAST, scd: FnopAST) extends FnopAST
case class Assignment(identifier: Identifier, expression: FnopExpression) extends FnopAST with FnopExpression {
}