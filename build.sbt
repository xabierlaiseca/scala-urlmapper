organization := "me.laiseca.urlmapper"

name := "url-mapper"

version := "0.0.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.0" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test"
)
