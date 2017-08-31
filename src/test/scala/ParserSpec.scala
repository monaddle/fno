package fno

import collection.mutable.Stack
import org.scalatest._
import fno._

import cats.data.WriterT
import cats.data.Writer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
import cats.implicits._
import cats.data
import shapeless.Id
import cats.syntax.writer._

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try


class ParserSpec extends FlatSpec with Matchers {

  "FNOParser" should "parse a declaration followed by a function and then a declaration" in {
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens,
      """
        class DockerContainer(imageName: String, runName: String, vm: VM)
        DockerContainer(imageName="library/hello-world", runName="hello", vm=VM(provider=DigitalOcean, os=Ubuntu1404))
      """.stripMargin)
    println(tokens)

    val andthen = FNOParser(tokens.get)
    andthen.isRight shouldBe true
    println(andthen)
    andthen.right map { x => x.isInstanceOf[AndThen] shouldBe true }
    println("and then", andthen)
    println("scd", andthen.right.get.checkTypes.run._2.asInstanceOf[TypedAndThen].scd)

    andthen.right.get.checkTypes.run._2.isInstanceOf[TypedAndThen] shouldBe true
    println(andthen.right.get.checkTypes.run._2.reify)
    val network = Reified.toNetwork(andthen.right.get.checkTypes.run._2.reify.toList)
    println(network)
    network.setupVMs
  }

  "FNOParser" should "parse a class declaration" in {
    val lexer = new fnop2
    val andthen =FNOParser(lexer.parse(lexer.tokens,
      """class DockerContainer(imageName: String, runName: String, vm: VM)
      """.stripMargin).get).right.get.checkTypes

    andthen.run._1 shouldBe Vector[String]()
  }
  "FNOParser" should "return a class doesn't exist error" in {
    val lexer = new fnop2
    val andthen =FNOParser(lexer.parse(lexer.tokens,
      """class DockerContainer(imageName: String, runName: String, vm: VM) extends foo
      """.stripMargin).get).right.get.checkTypes

    andthen.run._1 shouldBe Vector("Parent class foo isn't defined.")
    println(andthen)
  }

  "FNOParser" should "parse a class declaration with a function definition" in {
    val classdef =
      """class DigitalOcean() {
        |  def createVM(opts: DigitalOceanVMOpts) {
        |    CMD "curl google.com"
        |  }
        |}
      """.stripMargin

    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, classdef)
    println(tokens)

    val andthen = FNOParser(tokens.get)
    println(andthen)
  }

  "FNOParser" should "recognize an empty block" in {
    val block = "{ }"
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, block)
    println("hello!!", tokens)
    val andthen = FNOParser.block(new FnopReader(tokens.get))
    andthen.get shouldBe Block(Nil)
  }

  "FNOParser" should "recognize a function invocation" in {
    val cmd = """CMD ("curl google.com") """
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, cmd)
    val andthen = FNOParser.functionInvocation(new FnopReader(tokens.get))
    println("recognize function invocation", andthen)
    andthen.successful shouldBe true

  }

  "FNOParser" should "recognize a block with a function invocation" in {

    val block = s"""{ CMD ("curl google.com") }"""
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, block)
    val andthen = FNOParser.block(new FnopReader(tokens.get))
    println(tokens)
    println("recognize invocation in block", andthen)
    andthen.get.contains.head.isInstanceOf[FunctionInvocation] shouldBe true
  }

  "FNOParser" should "recognize an args definition" in {
    val argsDef =
      """
        (arg1: String, arg2: Int)
      """.stripMargin
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, argsDef)
    val andthen = FNOParser.argsDefinitions(new FnopReader(tokens.get))
    println("parsing args definition", andthen)
    andthen.get.head shouldBe ArgumentDefinition(Identifier("arg1"), Identifier("String"))
  }

  "FNOParser" should "recognize a function" in {
    val func = """ def createVM(opts: DigitalOceanVMOpts) {
                    CMD ("curl google.com")
                 }""".stripMargin
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, func)
    val andthen = FNOParser.functionDef(new FnopReader(tokens.get))
    println("parsing function", andthen)
    andthen.successful shouldBe true
  }

  "FNOParser" should "recognize a block with a function definition" in {
    val block =
      """{
        | def createVM(opts: DigitalOceanVMOpts) {
        |   CMD ("curl google.com")
          }
        }""".stripMargin
    val lexer = new fnop2
    val tokens = lexer.parse(lexer.tokens, block)
    println("hello!!", tokens)
    val andthen = FNOParser.block(new FnopReader(tokens.get))
    println("block with a function definition", andthen)
  }
}