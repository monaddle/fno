package fno

/**
  * Created by danielporter on 8/15/17.
  */
trait TypedFnopAST {
  val t: String

  def getHosts: Seq[String] = {
    this match {
      case td @ TypedDeclaration(id, expression, tp) => expression.getHosts
    }
  }

  def reify: Seq[Reified] = {
    this match {
      case TypedDeclaration(id, expression, t) => expression.reify
      case TypedIdentifier(id, t) => Seq(FnopAST.otherSymbolTable(id))
      case TypedLiteral(value, t) => Seq(StringLiteral(value))
      case TypedInstantiation(id, params, t) =>
        val paramMap = params map(param => (param.identifier.value, param.reify.asInstanceOf[Seq[ConcreteType]].head)) toMap

        Seq(ClassInstance(id.value, FnopAST.otherSymbolTable(id.value).asInstanceOf[Class], paramMap))
      case TypedAssignment(id, expression, t) => expression.reify
      case TypedAndThen(fst, scd, _) => fst.reify ++ scd.reify
      case x: Class => Seq(x)
    }
  }
}

trait TypedFnopExpression {
  val t: String
}

case class TypedDeclaration(id: TypedDeclaration, expression: TypedFnopAST, t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedIdentifier(value: String, t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedLiteral(value: String, t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedAndThen(fst: TypedFnopAST, scd: TypedFnopAST, t: String="") extends TypedFnopAST
case class TypedInstantiation(identifier: Identifier, params: List[TypedAssignment], t: String) extends TypedFnopAST with TypedFnopExpression
case class TypedAssignment(identifier: Identifier, expression: TypedFnopAST, t: String) extends TypedFnopAST with TypedFnopExpression

