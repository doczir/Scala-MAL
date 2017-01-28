name := "mal"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.typelevel" %% "cats" % "0.9.0"
)
