name := "scv-open"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings
addCompilerPlugin(scalaMacros)
libraryDependencies ++= testDeps

//
// open-cv specific
//

lazy val javaCvVer = "1.1"
lazy val tessVer   = s"3.04-$javaCvVer"
lazy val leptoVer  = s"1.72-$javaCvVer"
lazy val openCvVer = s"3.0.0-$javaCvVer"
lazy val os        = Sys.os.name

// append the os name onto the version to keep track of native stuff
version.transform(
  v => s"${v}_$os",
  sbt.NoPosition
)

libraryDependencies ++= Seq(
  "org.bytedeco"                 % "javacpp"   % javaCvVer,
  "org.bytedeco.javacpp-presets" % "tesseract" % tessVer,
  "org.bytedeco.javacpp-presets" % "tesseract" % tessVer classifier os,
  "org.bytedeco.javacpp-presets" % "leptonica" % leptoVer,
  "org.bytedeco.javacpp-presets" % "leptonica" % leptoVer classifier os,
  "org.bytedeco.javacpp-presets" % "opencv"    % openCvVer,
  "org.bytedeco.javacpp-presets" % "opencv"    % openCvVer classifier os,
  "org.bytedeco"                 % "javacv"    % javaCvVer
)

//
// ABSOLUTELY NECESSARY for all of the java-cpp based resources to correctly resolve !!!
//
classpathTypes += "maven-plugin"
//
// DO NOT REMOVE this above line !!!
//

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true
