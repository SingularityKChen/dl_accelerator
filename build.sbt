organization := "SingularityKChen"

name := "dl_acclerator"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11")

libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.2-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5",
  "com.lihaoyi" %% "utest" % "latest.integration"
)