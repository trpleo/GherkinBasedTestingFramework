import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / organization     := "io.ipapp"
ThisBuild / organizationName := "ebdd"

version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "utils",
    libraryDependencies ++= Seq(
      "com.lightbend.lagom" %% "lagom-scaladsl-testkit" % "1.4.11",

      "org.scalatest" %% "scalatest" % "3.0.4",

      "io.cucumber" %% "cucumber-scala" % "2.0.1",
      "io.cucumber" % "cucumber-core"   % "2.4.0",
      "io.cucumber" % "cucumber-junit"  % "2.4.0",
      "io.cucumber" % "cucumber-java8"  % "2.4.0",

      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.typelevel" %% "cats-effect" % "1.2.0"
    )
  )

enablePlugins(CucumberPlugin)

CucumberPlugin.glue := "steps"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
