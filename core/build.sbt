name := "unu-core"

version := "0.1"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.typelevel" %% "spire" % "0.14.1",
  "com.chuusai" %% "shapeless" % "2.3.2" % "test" /* for illTyped */
)

scalacOptions ++= Seq("-feature", "-deprecation")