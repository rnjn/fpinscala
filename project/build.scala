import sbt._
import Keys._

object FPInScalaBuild extends Build {

  val scalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  
  val options = Project.defaultSettings ++ Seq(

    organization := "in.rnjn",
    version := "0.1.0",
    scalaVersion := "2.11.6",
    sbtVersion := "0.13.6"
  )

  lazy val root =
    Project(id = "fpinscala",
      base = file("."),
      settings = options
    ) aggregate(exercises)

  lazy val exercises =
    Project(id = "exercises",
      base = file("exercises"),
      settings = options ++ Seq(
        libraryDependencies += scalaTest
      ))
}

