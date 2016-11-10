name := "unu-core"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.spire-math" %% "spire" % "0.11.0"
)
