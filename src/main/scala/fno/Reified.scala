package fno

import scala.collection.immutable.HashSet

/**
  * Created by danielporter on 8/15/17.
  */
trait Reified {
  def getInstancesOfType(searchName: String): List[ClassInstance] = {
    this match {
      case inst @ ClassInstance(name, cls, arguments) => if(cls.name==searchName) {
        List(inst)
      } else {
        Nil
      } ++ arguments flatMap { case(name, value) => value.getInstancesOfType(searchName)}
      case StringLiteral(_) => Nil
      case IntLiteral(_) => Nil
      case _ => Nil
    }
  }

  def getType: String = {
    this match {
      case Class(_, _, _, t) => t
      case ClassInstance(_, clz, _) => clz.getType
      case StringLiteral(_) => "String"
      case IntLiteral(_) => "Int"
      case Any => "Any"
    }
  }
}


case class Class(name: String, params: Map[Identifier, String], parentClass: Option[Identifier], t: String="") extends Reified with TypedFnopAST
case class ClassInstance(name: String, `class`: Class, vals: Map[String, ConcreteType]) extends Reified with ConcreteType
case class StringLiteral(value: String) extends Reified with ConcreteType
case class IntLiteral(value: Int) extends Reified with ConcreteType
case object Any extends Reified with ConcreteType

object Reified {
  def toNetwork(instances: List[Reified]): InfrastructureGraph = {
    val dockerImages = instances flatMap { x => x.getInstancesOfType("DockerContainer")} map {x => DockerContainer(x.vals)}
    println("instances", instances.head)
    println(dockerImages)
    val network = dockerImages map { img => dockerImageToGraph(img) } reduceRight (_ + _)
    network
  }

  def dockerImageToGraph(dockerImage: DockerContainer) = {
    val host = dockerImage.params("vm").asInstanceOf[ClassInstance]
    val provider = Provider(host.vals("provider").asInstanceOf[ClassInstance].name)
    val os = host.vals("os").asInstanceOf[ClassInstance]

    val osNode = OS(os.vals("name").asInstanceOf[StringLiteral].value, os.vals("version").asInstanceOf[StringLiteral].value)

    val vm = VM(host.vals)
    HashSet
    val a = HashSet("", "2", "3")
    val b = HashSet(provider, vm)
    val nodes: Set[Node] = Set(vm, provider, osNode, dockerImage)
    val edges: Set[Edge] = Set(Hosts(vm, dockerImage), Contains(provider, vm))

    InfrastructureGraph(nodes, edges)


  }
}

trait ConcreteType extends Reified{

}


