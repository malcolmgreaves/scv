import sbt._
import Keys._

object SharedBuild {

  val mgOrg = "io.malcolmgreaves"

  // // // // // // // //
  // //   Versions  // //
  // // // // // // // //
  
  lazy val macroV  = "2.1.0"
  lazy val sutilV = "0.2.0"

  // // // // // // // // // //
  // //    Dependencies   // //
  // // // // // // // // // //

  lazy val scalaMacros =
    "org.scalamacros" % "paradise" % macroV cross CrossVersion.full

  lazy val fpDeps = Seq(
    mgOrg                   %% "s-util-fp"     % sutilV    
    ,"com.github.mpilquist" %% "simulacrum"    % "0.7.0"  
  )

  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

  // // // // // // // // // //
  // //     Publishing    // //
  // // // // // // // // // //

  case class RepoInfo(group: String, name:  String)

  lazy val doPublish = (ri: RepoInfo) => Seq(
    publishMavenStyle        := true
    ,isSnapshot              := false
    ,publishArtifact in Test := false
    ,publishTo               := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }
    ,pomIncludeRepository    := { _ => false }
    ,pomExtra                := {
      <url>https://github.com/{ ri.group }/{ ri.name }</url>
        <licenses>
          <license>
            <name>Apache 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            <distribution>Yes</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:{ ri.group }/{ ri.name }.git</url>
          <connection>scm:git@github.com:{ ri.group }/{ ri.name }.git</connection>
        </scm>
        <developers>
          <developer>
            <id>malcolmgreaves</id>
            <name>Malcolm W. Greaves</name>
            <email>greaves.malcolm@gmail.com</email>
            <url>http://malcolmgreaves.io/</url>
          </developer>
        </developers>
    }
    ,publishArtifact         := true
  )

  lazy val noPublish = Seq(
    isSnapshot               := true
    ,publishArtifact in Test := false
    ,publishTo               := None
    ,pomIncludeRepository    := { _ => false }
    ,pomExtra                := { <nothing></nothing> }
    ,publishLocal            := {}
    ,publish                 := {}
    ,publishArtifact         := false
  )

  //
  // Misc.
  //

  lazy val allResolvers = Seq(
    // sonatype, maven central
    "Sonatype Releases"   at "https://oss.sonatype.org/content/repositories/releases/"
    ,"Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

    // bintray
    ,"Scalaz Bintray" at "http://dl.bintray.com/scalaz/releases"
  )


  /**
    * Adds functionality to obtain a unique, platform & operating specific string.
    * Necessary for downloading jars with platform-dependent native code.
    *
    * To get the current operating system (an instance of `OsType`), call `Sys.os`.
    * To use this value to add-on to an existing library dependency so that one may
    * obtain a platform-specific version, import Sys.Implicits. Then use `%` on a
    * `sbt.ModuleID` instance w/ the appropriate `OsType`.
    */
  object Sys {

    import scalaz.\/

    /**
      * Evaluates to an appropriate OsType, calculated internally from the
      * runtime's system and architecture string (from Sys.osStr).
      *
      * Throws a runtime MatchError if the value from osStr is completely
      * unrecognizable.
      */
    def os: OsType =
      parseOs(osStr).fold(e => throw e, identity)

    /** The runtime's system-specific identifying string. */
    def osStr: String =
      sys
        .props
        .getOrElse("os_override", System.getProperty("os.name"))
        .toLowerCase

    /**
      * Converts a string into a specific OsType. If the string matching is not
      * able to resolve the input to a known OSType, the the function evaluates
      * to a Throwable, describing how `parseOs` is unable to make progress.
      */
    lazy val parseOs: String => Throwable \/ OsType = {
      val osxRegex = """(mac)?\ ?os\ ?x?""".r
      val windowsRegex = """(microsoft)?\ ?windows.*""".r

      s => \/.fromTryCatch {
        s match {
          case "linux"         => Linux
          case osxRegex(_)     => Osx
          case windowsRegex(_) => Windows
        }
      }
    }

    object Implicits {

      /** Gives familiar sbt syntax to support platform-specific dependencies. */
      implicit class OsModuleID(val id: ModuleID) extends AnyVal {
        @inline def %(os: OsType): ModuleID =
          id.copy(revision = s"${id.revision}_${os.name}")
      }
    }

  }

  /** The unique identifying name of platform and architecture. */
  sealed abstract class OsType(val name: String) {
    /** Always evaluates to name. */
    final override val toString: String = name
  }
  // implementations
  case object Windows extends OsType(name="windows-x86")
  case object Osx extends OsType(name="macosx-x86_64")
  case object Linux extends OsType(name="linux-x86_64")

}
