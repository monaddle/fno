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

class PrestwoodParserSpec extends FlatSpec with Matchers {

  "Typechecker" should "identify one class definition" in {
    val tokens = PrestwoodLexer(
      """
        class DockerContainer(imageName: String, runName: String, vm: VM)
        DockerContainer(imageName="library/hello-world", runName="hello", vm=VM(provider=DigitalOcean, os=Ubuntu1404))
      """.stripMargin)
    println(tokens)

    val andthen = PrestwoodParser(tokens.get)
    andthen.right.get
    println("collected class declarations", andthen.right.get.cata(PrestwoodAST.collectClassDeclarations))
  }
}