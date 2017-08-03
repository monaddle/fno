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