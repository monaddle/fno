name := "eitiab"

version := "1.0"


resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"

scalaVersion := "2.12.2" // Doodle is currently published for Scala 2.12 only
resolvers += Resolver.bintrayRepo("underscoreio", "training")
libraryDependencies += "underscoreio" %% "doodle" % "0.8.2"

libraryDependencies += "de.sciss" % "sphinx4-core" % "1.0.0"

libraryDependencies += "de.sciss" % "sphinx4-data" % "1.0.0"
