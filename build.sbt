organization := "me.laiseca.urlmapper"

name := "url-mapper"

version := "0.1.0-SNAPSHOT"

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/xabierlaiseca/scala-urlmapper/"))

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.0" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test"
)

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

pomIncludeRepository := { _ => false }

pomExtra := (
    <scm>
      <url>git@github.com:xabierlaiseca/scala-urlmapper.git</url>
      <connection>scm:git:git@github.com:xabierlaiseca/scala-urlmapper.git</connection>
    </scm>
    <developers>
      <developer>
        <id>xabierlaiseca</id>
        <name>Xabier Laiseca</name>
        <url>http://laiseca.me</url>
      </developer>
    </developers>)
