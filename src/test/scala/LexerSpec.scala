/**
  * Created by danielporter on 8/15/17.
  */
import fno._
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {
  "lexer" should "return classkeyword" in {
    val lexer = new fnop2
    val res = lexer.parse(lexer.tokens, "class")
    res.successful should be (true)
    res.get.head should be (CLASSKEYWORD)
  }

  "lexer" should "return correct tokens for class inheritance" in {
    val lexer = new fnop2
    val res=  lexer.parse(lexer.tokens,
      """class DockerContainer(imageName: String, runName: String, vm: VM) extends foo
      """.stripMargin)
    println(res)
    res.successful shouldBe true
  }
}
