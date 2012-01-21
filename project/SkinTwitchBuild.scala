import sbt._
import Keys._
import Process._
import scala.collection.immutable._
import java.io.File

object BuildSettings {
  val buildOrganization = "com.github.skintwitch"
  val buildScalaVersion = "2.9.1"
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
  val artenum = "Artenum" at 
    "http://maven.artenum.com/content/repositories/thirdparty/"
  val allResolvers = Seq(scalaToolsSnapshots, ondex, migLayout, artenum)
}

object Dependencies {
  val scalaSwing  = "org.scala-lang" % "scala-swing" % "2.9.1"
  val scalaTest   = "org.scalatest" %% "scalatest" % "1.6.1" % "test"
  val jCommon     = "org.jfree" % "jcommon" % "1.0.17"
  val jFreechart  = "org.jfree" % "jfreechart" % "1.0.14"
  val xmlGraphics = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.4"
  val iText       = "com.lowagie" % "itext" % "2.1.5" intransitive()
  val migLayout   = "com.miglayout" % "miglayout-swing" % "4.2"
  val liftjson    = "net.liftweb" %% "lift-json" % "2.4"
  val ejml        = "com.googlecode.efficient-java-matrix-library" % "ejml" %
                        "0.18"
  val vtk         = "vtk" % "vtk" % "5.6.1"
  val allDependencies = Seq(
    scalaSwing, scalaTest, jCommon, jFreechart, xmlGraphics, iText, migLayout,
    liftjson, ejml, vtk
  )
}

object SkinTwitchBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Fetches native VTK files for Linux (x86_32, glibc2_7)
  def vtkNativeLinux = Command.command("vtkNativeLinux") { state =>
    val vtkRepo = "http://maven.artenum.com/content/repositories/thirdparty/" +
      "vtk/vtk-native/5.6.1/"
    val vtkNativeURL = vtkRepo + 
      "vtk-native-5.6.1-linux-x86_32-glibc2_7-jvm1_6.tgz"
    val destFile = new File("./lib/vtk-native.tgz")
    IO.download(new URL(vtkNativeURL), destFile)
    def untar() {
      "tar -xzf ./lib/vtk-native.tgz -C ./lib" !
    }
    untar
    state
  }
  
  // Fetches native VTK files for OS X (x86_64)
  def vtkNativeOSX = Command.command("vtkNativeOSX") { state =>
    val vtkRepo = "http://maven.artenum.com/content/repositories/thirdparty/" +
      "vtk/vtk-native/5.6.1/"
    val vtkNativeURL = vtkRepo + "vtk-native-5.6.1-osx-x86_64.zip"
    val destFile = new File("./lib/vtk-native.zip")
    IO.download(new URL(vtkNativeURL), destFile)
    def unzip() {
      "unzip -j ./lib/vtk-native.zip -d ./lib" !
    }
    unzip
    state
  }

  val vtkNativePath = if (System.getProperty("os.name") == "Mac OS X") {
    "-Djava.library.path=./lib/"
  } else {
    "-Djava.library.path=./lib/vtk-native-5.6.1-linux-x86_32-glibc2_7-jvm1_6/"
  }
  //println("vtkNativePath = %s" format vtkNativePath)
  val extraHeap = "-Xmx2048M"
  val runOptions = Seq(vtkNativePath, extraHeap)
  lazy val skintwitch = Project("skintwitch", file("."), 
				settings = buildSettings ++ Seq(
				  libraryDependencies := allDependencies,
				  resolvers := allResolvers,
				  fork in run := true,
				  javaOptions in run ++= runOptions,
          commands ++= Seq(vtkNativeLinux, vtkNativeOSX)
				)) dependsOn(scalaSignal, mocaputils, scalari)
	// can add scalacOptions := Seq("-unchecked", "-deprecation")
  val scalaSignalUri = uri("git://github.com/lancelet/scalasignal.git")
  lazy val scalaSignal = RootProject(scalaSignalUri)
  val mocaputilsUri = uri("git://github.com/lancelet/mocaputils.git")
  lazy val mocaputils = RootProject(mocaputilsUri)
  val scalariUri = uri("git://github.com/lancelet/scalari.git")
  lazy val scalari = RootProject(scalariUri)
}
