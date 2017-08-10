import collection.mutable.Stack
import org.scalatest._
import eitiab.main.{FNOParser, fnop2}
import eitiab.main._



class ParserSpec extends FlatSpec with Matchers {

  "FNOParser" should "parse things" in {
    val lexer = new fnop2
    val andthen =FNOParser(lexer.parse(lexer.tokens,
      """DockerImage(imageName="library/hello-world", runName="hello") deployedOn VM(os=Ubuntu1404, provider=DigitalOcean)
      """.stripMargin).get).right.get
    println(andthen)

    andthen should be (
      AndThen(
        Instantiation(
          Identifier("DockerImage"),
          List(
            Assignment(Identifier("imageName"), Literal("library/hello-world")),
            Assignment(Identifier("runName"),Literal("hello")))
        ),
        AndThen(
          Identifier("deployedOn"),
          Instantiation(Identifier("VM"),
            List(Assignment(Identifier("os"),
              Identifier("Ubuntu1404")),
              Assignment(Identifier("provider"),
                Identifier("DigitalOcean"))))))
    )
  }
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}