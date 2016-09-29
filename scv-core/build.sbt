name := "scv-core"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings
addCompilerPlugin(scalaMacros)
libraryDependencies ++= testDeps

//
// core specific
//

libraryDependencies ++= fpDeps

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true
