/*
package eitiab.main

import scala.collection.SortedMap

sealed trait Screen {

  val clearScreen: Int = new ProcessBuilder("clear").inheritIO.start.waitFor

  val msg: String
  val run: Unit = {
    this match {
      case Welcome => ClearScreen andThen
        ShowPrompt(Welcome.msg) andThen
        ReadLine andThen ShowMessages(Welcome.text) andThen Welcome.decision run
    }
  }
}
case object Welcome extends Screen {
  lazy val msg =
    """You wake up shivering, your hands burning daggers, the right half of your
      |face numb from prolonged contact with the desk. You slowly start to
      |remember your name...
      |
      |Enter your name: """.stripMargin
  lazy val msg2 =
    """|While taking inventory of your surroundings, you notice that the only item
       |on the desk is a computer. You also notice that the badge attached to your
       |belt says "Jerome M., Director of IT at Medi WeightLoss." You remember that
       |your name is Jerome.""".stripMargin
  lazy val msg3 =
    """Bewildered, you glance at the computer screen. There is an email open.
      |You smell a faint trace of burnt toast. You read the email: """.stripMargin
  lazy val msg4 =
    """    Sup, dawg. Like you asked, Linsdey from HR fired the entire cloud team
      |    yesterday, and had them wipe all of the cloud accounts. I don't know what's
      |    gotten into you, but I love it. Bro, you should have seen how mad
      |    the sysadmins were. Alex just kept muttering "What the fuck, what the fuck,
      |    what the fuck." lol. Can't stop progress, baby. Looking forward to seeing
      |    that CI environment and application you said you'd deploy today. Hate to
      |    fire you, too, lol. hope you still have those cloud credentials.""".stripMargin
  lazy val msg5 =
    """You remain bewildered. You look around outside of your cubicle: you're in a room filled with cubicles. It's deathly silent."""
  lazy val msg6 = """You look around in all directions. You can't see an end to the rows of cubicles. What do you do?""".stripMargin
  lazy val text = Seq(msg2, msg3, msg4, msg5, msg6)
  lazy val decision = Decision(Seq(Stay, WalkAway))
}

case object StarveToDeath extends Screen {
  lazy val msg = """You wander for hours, and then days. The cubicles never end. Your throat becomes parched. You perish."""
}*/
