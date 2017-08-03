/*
package eitiab.main

import scala.collection.SortedMap
import scala.io.StdIn
import scala.util.Try

/**
  * Created by danielporter on 7/11/17.
  */


sealed trait ConsoleAction {
  def run: String = {
    this match {
      case ReadChar => val char = Try(StdIn.readChar())
        char map(_.toString) getOrElse ""
      case ReadLine => val res = StdIn.readLine
        if (res.length == 0)
          ShowMessage("\r\nYou didn't enter anything. Try again:") andThen ReadLine run
        else{
          println()
          res
        }
      case AnyKeyToContinue => Try(ReadChar.run)
        ""
      case ShowMessage(msg) =>
        println(msg)
        ""
      case ShowPrompt(msg) => print(msg)
        ""
      case AndThen(first, second) => first.run
        second.run
      case ClearScreen =>  new ProcessBuilder("clear").inheritIO.start.waitFor
        println("======= Choose Your Own Adventure: Enterprise Edition ======\r\n")
        ""
      case ShowMessages(messages) => messages map { ShowMessage(_) andThen AnyKeyToContinue run }
        ""

      case Decision(choices: Map[Char, Action]) => choices foreach { case (letter, choice) => println(s"    -$letter: ${choice.name}")}
        val getChoice = ShowPrompt("Select an option: ") andThen ReadChar run

        if(getChoice == "" || !choices.keys.toList.contains(getChoice.charAt(0))) {
          ShowPrompt("Invalid selection. Please try again: ") andThen Decision(choices) run

        }
        else {
          getChoice
        }

        //getOrElse { ShowMessage("Invalid selection. Please try again: ") run}


      case ValidatedChar(chars) => val res = StdIn.readChar
        if(chars.contains(res)) res.toString
        else println("Oops. That wasn't a valid option. Please try again:")
        ValidatedChar(chars) run
    }
  }

  def andThen(second: ConsoleAction): AndThen = {
    AndThen(this, second)
  }
}

case object ReadChar extends ConsoleAction
case object ReadLine extends ConsoleAction
case object AnyKeyToContinue extends ConsoleAction
case object ClearScreen extends ConsoleAction
case object Continue extends ConsoleAction
case class ShowPrompt(msg: String) extends ConsoleAction
case class ShowMessages(messages: Seq[String]) extends ConsoleAction
case class Decision(choices: Map[Char, Choice]) extends ConsoleAction
object Decision {
  def apply(choices: Seq[Choice]): Decision = Decision((('a' to 'z') zip choices).toMap)
}
case class ValidatedChar(chars: Seq[Char]) extends ConsoleAction
case class ShowMessage(msg: String) extends ConsoleAction

case class AndThen(first: ConsoleAction, second: ConsoleAction) extends ConsoleAction
*/
