enablePlugins(ScalaJSPlugin)

name := """perf-ctorOpt"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "com.storm-enroute" %%% "scalameter" % "0.8-SNAPSHOT"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

