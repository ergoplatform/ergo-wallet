name := "ergo-wallet"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

val sigmaStateVersion = "master-2b4b07a1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "sigma-state" % sigmaStateVersion,
  "org.scodec" %% "scodec-bits" % "1.1.6",

  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
)
