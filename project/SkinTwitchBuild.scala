import sbt._
import sbt.Keys._
import sbt.Process._
import scala.collection.immutable._
import java.io.File

object BuildSettings {
  val buildOrganization = "com.github.skintwitch"
  val buildScalaVersion = "2.10.3"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization    := buildOrganization,
    scalaVersion    := buildScalaVersion,
    version         := buildVersion
  )
}

object Resolvers {
  //val migLayout = "MigLayout" at
  //  "http://www.miglayout.com/mavensite/"
  val artenum = "Artenum" at 
    "http://maven.artenum.com/content/repositories/thirdparty/"
  val sonatype = "Sonatype" at
  	"https://oss.sonatype.org/content/groups/public"
  val allResolvers = Seq(artenum, sonatype)
}

object Dependencies {
  val scalaSwing  = "org.scala-lang" % "scala-swing" % "2.10.3"
  val scalaTest   = "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  val jCommon     = "org.jfree" % "jcommon" % "1.0.17"
  val jFreechart  = "org.jfree" % "jfreechart" % "1.0.14"
  val xmlGraphics = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.4"
  val iText       = "com.lowagie" % "itext" % "2.1.5" intransitive()
  val migLayout   = "com.miglayout" % "miglayout-swing" % "4.2"
  val liftjson    = "net.liftweb" %% "lift-json" % "2.5.1"
  val ejml        = "com.googlecode.efficient-java-matrix-library" % "ejml" %
                        "0.18"
  val vtk         = "vtk" % "vtk" % "5.8.0"
  val breeze      = "org.scalanlp" %% "breeze-math" % "0.3-SNAPSHOT"
  val actors      = "org.scala-lang" % "scala-actors" % "2.10.3"
  val allDependencies = Seq(
    scalaSwing, scalaTest, jCommon, jFreechart, xmlGraphics, iText, migLayout,
    liftjson, ejml, vtk, breeze, actors
  )
}

object SkinTwitchBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Fetches native VTK files for Linux (x86_32, glibc2_7)
  def vtkNativeLinux = Command.command("vtkNativeLinux") { state =>
    val vtkRepo = "http://maven.artenum.com/content/repositories/thirdparty/" +
      "vtk/vtk-native/5.8.0/"
    val vtkNativeURL = vtkRepo + 
      "vtk-native-5.8.0-linux-x86_64.zip"
    val destFile = new File("./lib/vtk-native.zip")
    IO.download(new URL(vtkNativeURL), destFile)
    def unzip() {
      import sbt.Process._
      "unzip -j ./lib/vtk-native.zip -d ./lib" !
    }
    unzip
    state
  }
  
  // Fetches native VTK files for OS X (x86_64)
  def vtkNativeOSX = Command.command("vtkNativeOSX") { state =>
    val vtkRepo = "http://maven.artenum.com/content/repositories/thirdparty/" +
      "vtk/vtk-native/5.8.0/"
    val vtkNativeURL = vtkRepo + "vtk-native-5.8.0-osx-x86_64.zip"
    val destFile = new File("./lib/vtk-native.zip")
    IO.download(new URL(vtkNativeURL), destFile)
    def unzip() {
      import sbt.Process._
      "unzip -j ./lib/vtk-native.zip -d ./lib" !
    }
    unzip
    state
  }

  val vtkNativePath = "-Djava.library.path=./lib/"
  
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
