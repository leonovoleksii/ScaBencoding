name := "ScaBencoding"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.10.0")

scalacOptions in Test ++= Seq("-Yrangepos")

// scalacOptions += "-Ypartial-unification"