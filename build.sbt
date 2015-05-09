name := "free-applicative-cats"

organization := "com.alexknvl"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.spire-math" %% "algebra" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra_2.11-0.2.0-SNAPSHOT.jar",
  "org.spire-math" %% "algebra-std" % "0.2.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/algebra-std_2.11-0.2.0-SNAPSHOT.jar",
  "org.spire-math" %% "cats-std" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-core" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-free" % "0.1.0-SNAPSHOT"
)