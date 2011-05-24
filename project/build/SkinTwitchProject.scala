import sbt._
import de.element34.sbteclipsify._

class SkinTwitchProject(info: ProjectInfo) extends DefaultProject(info)
with Eclipsify {

  val scalaToolsSnapshots = "Scala Tools Snapshots" at 
    "http://scala-tools.org/repo-snapshots"
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"

  val scalaTest = "org.scalatest" %% "scalatest" % "1.4.1"
  // val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8"

  // enable unchecked warnings
  //override def compileOptions = super.compileOptions ++ Seq(Unchecked)

}
