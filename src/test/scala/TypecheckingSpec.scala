import matryoshka.data.Fix
import org.scalatest._
import prestwood.{PrestwoodAST, PrestwoodLexer, PrestwoodParser}
import matryoshka._
import slamdata.Predef._

import scalaz._

import slamdata.Predef._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._

class TypecheckingSpec extends FlatSpec with Matchers {

  "Typechecker" should "identify one class definition" in {
    val tokens = PrestwoodLexer(
      """
        class DockerContainer(imageName: String, runName: String, vm: VM)
        class DockerContainer2(imageName: String, runName: String, vm: VM)
        class DockerContainer4(imageName: String, runName: String, vm: VM)
        DockerContainer3(imageName="library/hello-world", runName="hello", vm=VM(provider=DigitalOcean, os=Ubuntu1404))
      """.stripMargin)
    println(tokens)
    println("OMFG OMFG OMFG")
    val andthen = PrestwoodParser(tokens.get)
    val a =andthen.right.get
    println(a)

    println("collected class declarations", PrestwoodAST.a(andthen.right.get))
  }
}