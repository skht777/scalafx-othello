name := "othello"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.4"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.2.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

fork := true