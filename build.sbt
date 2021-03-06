name := ""

import SharedBuild.{ doPublish, noPublish, RepoInfo }
com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

scalaVersion in ThisBuild := "2.11.8"
organization in ThisBuild := "io.malcolmgreaves"
version in ThisBuild      := {
  val major: Int = 0
  val minor: Int = 0
  val patch: Int = 0
  s"$major.$minor.$patch"
}

lazy val root = project
  .in(file("."))
  .aggregate(
  	`scv-core`
    ,`scv-boof`
    ,`scv-open`
  )
  .settings { noPublish }

lazy val `scv-core` = project
  .in(file("scv-core"))
  .settings {
    doPublish { 
	    RepoInfo(group = "malcolmgreaves", name = "scv-core")
  	}
  }

lazy val `scv-boof` = project
  .in(file("scv-boof"))
  .dependsOn(`scv-core`)
  .settings {
    doPublish { 
	    RepoInfo(group = "malcolmgreaves", name = "scv-boof")
  	}
  }

lazy val `scv-open` = project
  .in(file("scv-open"))
  .dependsOn(`scv-core`)
  .settings {
    doPublish {
      RepoInfo(group = "malcolmgreaves", name = "scv-open")
    }
  }

lazy val subprojects: Seq[ProjectReference] = root.aggregate
lazy val publishTasks = subprojects.map { publish.in }

resolvers in ThisBuild := Seq(
  "Sonatype Releases"  at "https://oss.sonatype.org/content/repositories/releases/"
  ,"Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
	,"Scalaz Bintray" at "http://dl.bintray.com/scalaz/releases"
)

lazy val javaV = "1.8"
scalacOptions in ThisBuild := Seq(
	"-Xfatal-warnings" // Every warning is esclated to an error.
  ,"-optimize"
  ,"-deprecation"
  ,"-feature"
  ,"-unchecked"
  ,s"-target:jvm-$javaV"
  ,"-encoding"
  ,"utf8"
  ,"-language:postfixOps"
  ,"-language:existentials"
  ,"-language:higherKinds"
  ,"-language:implicitConversions"
  ,"-language:experimental.macros"
  ,"-language:reflectiveCalls"
  ,"-Yno-adapted-args"
  ,"-Ywarn-value-discard"
  ,"-Xlint"
  ,"-Xfuture"
  ,"-Ywarn-dead-code"
  ,"-Yinline-warnings"
)
javacOptions in ThisBuild := Seq("-source", javaV, "-target", javaV)
javaOptions in ThisBuild  := Seq(
  "-XX:+UseG1GC"
  ,"-server"
  ,"-XX:+AggressiveOpts"
  ,"-XX:+TieredCompilation"
  ,"-XX:CompileThreshold=420"
  ,"-Xmx3000M"
)
