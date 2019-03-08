import sbt._

object Version {
  final val scalaTestVersion = "3.0.5"
}

object Dependencies {

  import Version._
  
  lazy final val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
}
