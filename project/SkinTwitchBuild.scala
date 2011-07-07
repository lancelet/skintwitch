import sbt._
import Keys._
import scala.collection.immutable._

object BuildSettings {
  val buildOrganization = "com.github.skintwitch"
  val buildScalaVersion = "2.9.0"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization    := buildOrganization,
    scalaVersion    := buildScalaVersion,
    version         := buildVersion
    /*offline         := true*/
  )
}

object Resolvers {
  val scalaToolsSnapshots = "Scala-Tools Snapshots" at
    "http://scala-tools.org/repo-snapshots"
  val ondex = "Ondex" at
    "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val migLayout = "MigLayout" at
    "http://www.miglayout.com/mavensite/"
  val allResolvers = Seq(scalaToolsSnapshots, ondex, migLayout)
}

object Dependencies {
  val scalaSwing  = "org.scala-lang" % "scala-swing" % "2.9.0"
  val scalaTest   = "org.scalatest" %% "scalatest" % "1.4.1" % "test"
  val jCommon     = "jfree" % "jcommon" % "1.0.16"
  val jFreechart  = "jfree" % "jfreechart" % "1.0.13"
  val xmlGraphics = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1"
  val iText       = "com.lowagie" % "itext" % "2.1.5" intransitive()
  val migLayout   = "com.miglayout" % "miglayout" % "3.7.4" classifier "swing"
  val liftjson    = "net.liftweb" %% "lift-json" % "2.4-M2"
  val allDependencies = Seq(
    scalaSwing, scalaTest, jCommon, jFreechart, xmlGraphics, iText, migLayout,
    liftjson
  )
}

object SkinTwitchBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  val vtkNativePath = "-Djava.library.path=./lib/vtk-native-5.6.1-osx-x86_64"
  val extraHeap = "-Xmx4096M"
  val runOptions = Seq(vtkNativePath, extraHeap)
  lazy val skintwitch = Project("skintwitch", file("."), 
				settings = buildSettings ++ Seq(
				  libraryDependencies := allDependencies,
				  resolvers := allResolvers,
				  fork in run := true,
				  javaOptions in run ++= runOptions
				)) dependsOn(scalaSignal, mocaputils, scalari)
  val scalaSignalUri = uri("git://github.com/lancelet/scalasignal.git")
  lazy val scalaSignal = RootProject(scalaSignalUri)
  val mocaputilsUri = uri("git://github.com/lancelet/mocaputils.git")
  lazy val mocaputils = RootProject(mocaputilsUri)
  val scalariUri = uri("git://github.com/lancelet/scalari.git")
  lazy val scalari = RootProject(scalariUri)
}
