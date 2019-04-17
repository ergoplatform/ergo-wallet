name := "ergo-wallet"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

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
