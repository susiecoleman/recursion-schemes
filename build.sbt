name := "recursion-schemes"

scalacOptions += "-Ypartial-unification"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.slamdata" %% "matryoshka-core" % "0.18.3"
)