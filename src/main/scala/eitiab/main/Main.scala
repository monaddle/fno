/*
package eitiab.main

/**
  * Created by danielporter on 7/11/17.
  */
import scala.io.StdIn


import java.io.IOException

object CLS {
  @throws[IOException]
  @throws[InterruptedException]
  def main(args: Array[String]): Unit = {
    Welcome run
  }
}


trait Action




trait Choice {
  val next: Screen
  val name: String
}

case object WalkAway extends Choice {
  lazy val next = StarveToDeath
  lazy val name = "Try to find the exit."
}

case object Stay extends Choice {
  lazy val next = StarveToDeath
  lazy val name = "Stay and build a cloud environment."
}*/
