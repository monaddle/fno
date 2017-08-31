package fno

/**
  * Created by danielporter on 8/15/17.
  */
case class InfrastructureGraph(
                                nodes: Set[Node],
                                edges: Set[Edge],
                                edgesFromNode: Map[Node, Set[Edge]],
                                edgesToNode: Map[Node, Set[Edge]]) {
  def +(graph: InfrastructureGraph): InfrastructureGraph = {
    InfrastructureGraph(this.nodes ++ graph.nodes, this.edges ++ graph.edges)
  }

  def getNeighbors(node: Node): Iterable[Node] = (edgesFromNode(node) map {_.to}) ++ (edgesToNode(node) map{_.from})

  def getNeighborsOfType[T <: Node](node: Node): Iterable[T] =
    (edgesFromNode(node) map {_.to} collect {case x: T => x})++ (edgesToNode(node) map{_.from} collect {case x: T => x})

  def getProviders = nodes collect { case x: Provider => x}

  def setupVMs = {
    val providers = getProviders
    providers map { provider =>
      val vms = this.getNeighborsOfType[VM](provider)
      vms map provider.createVM
    }
  }
}
object InfrastructureGraph {
  def apply(nodes: Set[Node]= Set(), edges: Set[Edge] = Set()): InfrastructureGraph = {
    InfrastructureGraph(nodes,
      edges,
      edges groupBy( x=> x.from) withDefaultValue Set(),
      edges groupBy( x=> x. to) withDefaultValue Set()
    )
  }
}


case class Provider(name: String, graph: Option[InfrastructureGraph] = None) extends Node {
  val token = "8243e441b650c91200f84d05d6eff839f23147d594385ce3e4bf4a26629d4a80"
  import sys.process._
  def createVM(vm: VM) = {
    val cmd2 = s"""curl -X POST -H \\"Content-Type: application/json\\" -H "Authorization: Bearer $token" -d '{"name":"example.com","region":"nyc3","size":"512mb","image":"ubuntu-14-04-x64","ssh_keys":null,"backups":false,"ipv6":true,"user_data":null,"private_networking":null,"volumes": null,"tags":["web"]}' \\"https://api.digitalocean.com/v2/droplets\\""""
    val cmd = s"curl google.com"
    cmd !
  }
}




trait Node {
  def graph: Option[InfrastructureGraph]
}
trait Edge {
  val from: Node
  val to: Node
}

case class VM(params: Map[String, ConcreteType], graph: Option[InfrastructureGraph] = None) extends Node

trait Application extends Node
case class OS(name: String, version: String, graph: Option[InfrastructureGraph] = None) extends Node
case class ApplicationInstance(id: Identifier, params: Map[String, ConcreteType], graph: Option[InfrastructureGraph] = None) extends Node with Application
case class DockerContainer(params: Map[String, ConcreteType], graph: Option[InfrastructureGraph] = None) extends Node with Application
case class Network(name: String, graph: Option[InfrastructureGraph] = None) extends Node

case class Hosts(from: Node, to: Node) extends Edge
case class Contains(from: Node, to: Node) extends Edge
