import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "com.github.skintwitch"
  val buildScalaVersion = "2.9.0"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version      := buildVersion
  )
}

object Resolvers {
  val scalaToolsSnapshots = "Scala-Tools Snapshots" at
    "http://scala-tools.org/repo-snapshots"
  val ondex = "Ondex" at
    "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val allResolvers = Seq(scalaToolsSnapshots, ondex)
}

object Dependencies {
  val scalaSwing  = "org.scala-lang" % "scala-swing" % "2.9.0"
  val scalaTest   = "org.scalatest" %% "scalatest" % "1.4.1" % "test"
  val jCommon     = "jfree" % "jcommon" % "1.0.16"
  val jFreechart  = "jfree" % "jfreechart" % "1.0.13"
  val xmlGraphics = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1"
  val iText       = "com.lowagie" % "itext" % "2.1.5" intransitive()
  val allDependencies = Seq(
    scalaSwing, scalaTest, jCommon, jFreechart, xmlGraphics, iText
  )
}

object SkinTwitchBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val skintwitch = Project("skintwitch", file("."), 
				settings = buildSettings ++ Seq(
				  libraryDependencies := allDependencies,
				  resolvers := allResolvers
				)) dependsOn(scalaSignal, mocaputils)
  val scalaSignalUri = uri("git://github.com/lancelet/scalasignal.git")
  lazy val scalaSignal = RootProject(scalaSignalUri)
  val mocaputilsUri = uri("git://github.com/lancelet/mocaputils.git")
  lazy val mocaputils = RootProject(mocaputilsUri)
}
