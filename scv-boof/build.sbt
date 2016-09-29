name := "scv-boof"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings
addCompilerPlugin(scalaMacros)
libraryDependencies ++= testDeps

//
// boof-cv specific
//
libraryDependencies ++= Seq(
  "core"
  ,"feature"
  ,"io"
  ,"ip"
  ,"sfm"
  ,"visualize"
  ,"recognition"
  ,"learning"
  )
  .map { name =>
    "org.boofcv" % name % "0.24.1"
  }

libraryDependencies += "org.spire-math" %% "spire" % "0.12.0"

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true
