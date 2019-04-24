name := "ergo-wallet"

organization := "org.ergoplatform"

version := "0.1.0"

scalaVersion := "2.12.8"

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/"
)

val sigmaStateVersion = "master-2b4b07a1-SNAPSHOT"
val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "sigma-state" % sigmaStateVersion,
  "org.scodec" %% "scodec-bits" % "1.1.6",

  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
)

licenses in ThisBuild := Seq("CC0 1.0 Universal" -> url("https://github.com/ergoplatform/ergo-wallet/blob/master/LICENSE"))

homepage in ThisBuild := Some(url("https://github.com/ergoplatform/ergo-wallet"))

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:ergoplatform/ergo-wallet.git</url>
    <connection>scm:git:git@github.com:ergoplatform/ergo-wallet.git</connection>
  </scm>
    <developers>
      <developer>
        <id>Oskin1</id>
        <name>Ilya Oskin</name>
      </developer>
    </developers>
