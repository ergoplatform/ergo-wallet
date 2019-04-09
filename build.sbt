name := "ergo-wallet"

version := "0.1"

scalaVersion := "2.12.8"

val sigmaStateVersion = "master-2b4b07a1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "sigma-state" % sigmaStateVersion
)
