import sbt._
import de.element34.sbteclipsify._

class SkinTwitchProject(info: ProjectInfo) extends DefaultProject(info)
with Eclipsify {

  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.9.0"
  
  val scalaToolsSnapshots = "Scala Tools Snapshots" at 
    "http://scala-tools.org/repo-snapshots"
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"

  val scalaTest = "org.scalatest" %% "scalatest" % "1.4.1"
  // val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8"

  // ScalaLA stuff
  val scalaNLPRepo = "ScalaNLP" at "http://repo.scalanlp.org/repo"
  val ondexRepo = "ondex" at 
    "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalala = "org.scalala" %%  "scalala" % "1.0.0.RC2-SNAPSHOT"

  // Apache Commons Math
  val commonsMath = "org.apache.commons" % "commons-math" % "2.0"  
  
  // JFreeChart
  val JCommon = "jfree" % "jcommon" % "1.0.16"
  val JFreeChart = "jfree" % "jfreechart" % "1.0.13"
  val XMLGraphicsCommons = "org.apache.xmlgraphics" % "xmlgraphics-commons" % 
      "1.3.1"
  val IText = "com.lowagie" % "itext" % "2.1.5" intransitive()  
    
  // scalasignal and mocaputils
  val scalaSignal = "GitHub" %% "scalasignal" % "0.3-SNAPSHOT"
  val mocapUtils = "MEPC" %% "mocaputils" % "0.2-SNAPSHOT"
  
  // SignalML Repo (required for ScalaSignal / JTransforms)
  val signalmlRepo = "SignalMLRepo" at "http://signalml.org/maven/repository"
  
  // enable unchecked warnings
  //override def compileOptions = super.compileOptions ++ Seq(Unchecked)

}
