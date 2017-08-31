package fno
import cats.data.{Writer, WriterT}
import cats.syntax.writer._
import fno._
import shapeless.Id
import cats._
import cats.data._
import cats.implicits._

import scala.collection.mutable
import scala.util.Try

trait FnopExpression extends FnopAST
object FnopAST {

  val OSClass = Class(name="OS",
    params = Map(Identifier("name") -> "String", Identifier("version") -> "String"),
    None
  )

  val ProviderClass = Class(name="Provider",
    params = Map(),
    None
  )


  val otherSymbolTable = mutable.HashMap[String, Reified] (

    "VM" -> Class(name="VM",
      params = Map(
        Identifier("provider") -> "Provider",
        Identifier("os") -> "OS"),
      None
    ),
    "Provider" -> ProviderClass,
    "DigitalOcean" -> ClassInstance("DigitalOcean", ProviderClass, Map()),
    "OS" -> OSClass,
    "Ubuntu1404" -> ClassInstance("Ubuntu1404", OSClass, Map("name" -> StringLiteral("Ubuntu"), "version" -> StringLiteral("14.04"))))
}



trait FnopAST {
  type Logged[A] = WriterT[Id, Vector[String], A]
  def checkTypes: Logged[TypedFnopAST]= {
    this match {
      case Identifier(id) => val res = FnopAST.otherSymbolTable.getOrElse(id, Any)
        Writer(if(res.getType=="any") Vector(s"$id is not a valid identifier.") else Vector(),
          TypedIdentifier(id, res.getType)).asInstanceOf[Writer[Vector[String], TypedFnopAST]]
      case Instantiation(id, assignments) =>

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


        val res = typedAssignmentsW flatMap { assignments =>
          val typedInstantiation = TypedInstantiation(id, assignments.toList, id.value)
          val maybeClass = Try{FnopAST.otherSymbolTable.get(id.value).asInstanceOf[Option[Class]]}  getOrElse None
          maybeClass map { cls =>
            val a = assignments flatMap { assignment =>
              cls.params.get(assignment.identifier) map { paramType =>
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

        expression.checkTypes map { a=>
          FnopAST.otherSymbolTable += (id.value -> a.reify.head)
          TypedAssignment(id, a, a.t)}
      case ClassDeclaration(id, attrs, parent, block) => {
        val vector = parent map { p => FnopAST.otherSymbolTable.get(p.value) map { parentType =>
          Try {
            val parentParams = parentType.asInstanceOf[Class].params
            attrs flatMap { x=>
              parentParams.get(x.identifier) map { parentAttrType =>
                if(parentAttrType == x.givenType.value) Vector() else Vector(s"Attribute ${x.identifier} expected type is ${parentAttrType}, given type is ${x.givenType}")
              } getOrElse Vector()
            } toVector
          } getOrElse Vector(s"Parent class $parent is not a valid class.")
        } getOrElse Vector(s"Parent class ${p.value} isn't defined.")
        } getOrElse Vector()
        val cls = Class(id.value, attrs map {x=> (x.identifier, x.givenType.value)} toMap, parent)
        FnopAST.otherSymbolTable += cls.name -> cls
        Writer(vector, cls)
      }
      case AndThen(fst, scd) => for {
        a <- fst.checkTypes
        b <- scd.checkTypes
      } yield {TypedAndThen(a, b)}
    }
  }
}

case class Declaration(id: Identifier, expression: FnopExpression) extends FnopAST with FnopExpression
case class Identifier(value: String) extends FnopAST with FnopExpression
case class Literal(value: String) extends FnopAST with FnopExpression

case class Block(contains: List[FnopExpression]) extends FnopAST with FnopExpression
case class FunctionInvocation(id: Identifier, args: List[FnopExpression]) extends FnopAST with FnopExpression
case class FunctionDefinition(id: Identifier, args: List[ArgumentDefinition], block: Block) extends FnopAST with FnopExpression
case class Instantiation(`type`: Identifier, params: List[Assignment]) extends FnopAST with FnopExpression
case class AndThen(fst: FnopAST, scd: FnopAST) extends FnopAST
case class Assignment(identifier: Identifier, expression: FnopExpression) extends FnopAST with FnopExpression
case class ArgumentDefinition(identifier: Identifier, givenType: Identifier) extends FnopAST with FnopExpression
case class ClassDeclaration(identifier: Identifier, params: List[ArgumentDefinition], parent: Option[Identifier], block: Block) extends FnopAST with FnopExpression