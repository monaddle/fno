import matryoshka.data.Fix
import org.scalatest._
import prestwood.PrestwoodAST.{Block, FunctionInvocation, Id}
import prestwood.{PrestwoodAST, PrestwoodLexer, PrestwoodParser, PrestwoodReader}
import prestwood.{PrestwoodAST => PA}

class PrestwoodParserSpec extends FlatSpec with Matchers {

  "PrestwoodParser" should "parse an assignment" in {
    val tokens = PrestwoodLexer("""imageName="library/hello-world"""")
    val parseRes = PrestwoodParser.assignment(new PrestwoodReader(tokens.get))
    parseRes.successful shouldBe true
    println("parseres success", parseRes.get)
  }

  "PrestwoodParser" should "parse an assignment with an instantiation" in {
    val tokens = PrestwoodLexer("""vm=VM(provider=DigitalOcean, os=Ubuntu1404)"""")
    val parseRes = PrestwoodParser.assignment(new PrestwoodReader(tokens.get))
    parseRes.successful shouldBe true
    println("parseres success", parseRes.get)
  }

  "PrestwoodParser" should "parse an instantiation" in {
    val tokens = PrestwoodLexer(
      """
        DockerContainer(imageName="library/hello-world", runName="hello", vm=VM(provider=DigitalOcean, os=Ubuntu1404))
      """.stripMargin)

    val andthen = PrestwoodParser(tokens.get)
    println("and then", andthen)
    andthen.isRight shouldBe true
    val vm = PA.instantiation("VM", List(
      PA.assignment("provider", Fix(PA.ID("DigitalOcean"))),
      PA.assignment("os", Fix(PA.ID("Ubuntu1404")))
    ))
    val cd = PA.classDelcaration(PA.ID("DockerContainer"), List(PA.argumentDefinition(PA.ID("imageName"), PA.ID("String"))), parent = None, block = PA.block(Nil))
    val instance = PA.instantiation("DockerContainer", List(
      PA.assignment("imageName", PA.stringLiteral("library/hello-world")),
      PA.assignment("runName", PA.stringLiteral("hello")),
      PA.assignment("vm", vm)
    ))
    andthen map { x => x shouldBe instance }
  }

  "PrestwoodParser" should "parse a declaration followed by an instantiation" in {
    val tokens = PrestwoodLexer(
      """
        class DockerContainer(imageName: String, runName: String, vm: VM)
        DockerContainer(imageName="library/hello-world", runName="hello", vm=VM(provider=DigitalOcean, os=Ubuntu1404))
      """.stripMargin)
    println(tokens)

    val andthen = PrestwoodParser (tokens.get)
    andthen.isRight shouldBe true

    val vm = PA.instantiation("VM",List(
      PA.assignment("provider", Fix(PA.ID("DigitalOcean"))),
      PA.assignment("os", Fix(PA.ID("Ubuntu1404")))
    ))

    val cd = PA.classDelcaration(PA.ID("DockerContainer"), List(
      PA.argumentDefinition(PA.ID("imageName"), PA.ID("String")),
      PA.argumentDefinition(PA.ID("runName"), PA.ID("String")),
      PA.argumentDefinition(PA.ID("vm"), PA.ID("VM"))
    ), parent=None, block=PA.block(Nil))

    val instance = PA.instantiation("DockerContainer", List(
      PA.assignment("imageName", PA.stringLiteral("library/hello-world")),
      PA.assignment("runName", PA.stringLiteral("hello")),
      PA.assignment("vm", vm)
    ))
    andthen map { x=> x shouldBe PA.andThen(cd,instance
    )}
    /*println("scd", andthen.right.get.checkTypes.run._2.asInstanceOf[TypedAndThen].scd)

    andthen.right.get.checkTypes.run._2.isInstanceOf[TypedAndThen] shouldBe true
    println(andthen.right.get.checkTypes.run._2.reify)
    val network = Reified.toNetwork(andthen.right.get.checkTypes.run._2.reify.toList)
    println(network)
    network.setupVMs*/
  }

  "PrestwoodParser" should "parse a class declaration" in {
    val andthen =PrestwoodParser(PrestwoodLexer(
      """class DockerContainer(imageName: String, runName: String, vm: VM)
      """.stripMargin).get)

    andthen.right.get.isInstanceOf[Fix[PrestwoodAST.ClassDeclaration]] shouldBe true
  }


  "PrestwoodParser" should "parse a class declaration with a function definition" in {
    val classdef =
      """class DigitalOcean() {
        |  def createVM(opts: DigitalOceanVMOpts) {
        |    CMD "curl google.com"
        |  }
        |}
      """.stripMargin

    val tokens = PrestwoodLexer(classdef)
    println(tokens)

    val andthen = PrestwoodParser(tokens.get)
    println(andthen)
  }

  "PrestwoodParser" should "recognize an empty block" in {
    val block = "{ }"
    val tokens = PrestwoodLexer(block)
    println("hello!!", tokens)
    val andthen = PrestwoodParser.block(new PrestwoodReader(tokens.get))
    andthen.get.unFix shouldBe Block(Nil)

  }

  "PrestwoodParser" should "recognize a function invocation" in {
    val cmd = """CMD ("curl google.com") """
    val tokens = PrestwoodLexer(cmd)
    val andthen = PrestwoodParser.functionInvocation(new PrestwoodReader(tokens.get))
    println("recognize function invocation", andthen)
    andthen.get.unFix shouldBe FunctionInvocation(Id("CMD"), List(PrestwoodAST.stringLiteral("curl google.com")))
  }

  "PrestwoodParser" should "recognize a block with a function invocation" in {

    val block = s"""{ CMD ("curl google.com") }"""
    val tokens = PrestwoodLexer(block)
    val andthen = PrestwoodParser.block(new PrestwoodReader(tokens.get))
    println(tokens)
    println("recognize invocation in block", andthen)
    andthen.get.unFix shouldBe Block(List(PrestwoodAST.functionInvocation(Id("CMD"), List(PrestwoodAST.stringLiteral("curl google.com")))))
  }

  "PrestwoodParser" should "recognize an args definition" in {
    val argsDef =
      """
        (arg1: String, arg2: Int)
      """.stripMargin
    val tokens = PrestwoodLexer(argsDef)
    val andthen = PrestwoodParser.argsDefinitions(new PrestwoodReader(tokens.get))
    println("parsing args definition", andthen)
    andthen.get.head shouldBe PrestwoodAST.argumentDefinition(PrestwoodAST.ID("arg1"), PrestwoodAST.ID("String"))
  }


  "PrestwoodParser" should "recognize a function" in {
    val func = """ def createVM(opts: DigitalOceanVMOpts) {
                    CMD ("curl google.com")
                 }""".stripMargin
    val tokens = PrestwoodLexer(func)
    val andthen = PrestwoodParser.functionDef(new PrestwoodReader(tokens.get))
    println("parsing function", andthen)
    andthen.get shouldBe PA.functionDefinition(PA.ID("createVM"), List(
      PA.argumentDefinition(PA.ID("opts"), PA.ID("DigitalOceanVMOpts"))), PA.block(List(PrestwoodAST.functionInvocation(Id("CMD"), List(PrestwoodAST.stringLiteral("curl google.com"))))))
  }

  "PrestwoodParser" should "recognize a block with a function definition" in {
    val block =
      """{
        | def createVM(opts: DigitalOceanVMOpts) {
        |   CMD ("curl google.com")
          }
        }""".stripMargin
    val tokens = PrestwoodLexer(block)
    val andthen = PrestwoodParser.block(new PrestwoodReader(tokens.get))

    andthen.get shouldBe PA.block(List(PA.functionDefinition(PA.ID("createVM"), List(
      PA.argumentDefinition(PA.ID("opts"), PA.ID("DigitalOceanVMOpts"))), PA.block(List(PrestwoodAST.functionInvocation(Id("CMD"), List(PrestwoodAST.stringLiteral("curl google.com"))))))))
  }

  "PrestwoodParser" should "parse a variable declaration" in {
    val declaration = """val a = "assignment""""
    val res = PrestwoodParser(PrestwoodLexer(declaration).get)
    res.right.get shouldBe PA.declaration(PA.ID("a"), PA.stringLiteral("assignment"))
  }

  "PrestwoodParser" should "recognize a class with a function definition" in {
    val classWithFunc =
      """
        |class a(){
        | def a() {
        |
        | }
        |}
      """.stripMargin

    val res = PrestwoodParser(PrestwoodLexer(classWithFunc).get)
    res.right.get shouldBe PA.classDelcaration(PA.ID("a"), Nil, None, PA.block(List(PA.functionDefinition(PA.ID("a"), Nil, PA.block(Nil)))))
  }
}