package skintwitch.analysis

import scala.util.logging.ConsoleLogger
import scala.util.logging.Logged
import java.io.File

class Analysis extends Logged {

  // banner
  log("+-------------------------+")
  log("| Analysis of Skin Motion |")
  log("+-------------------------+")
  
  // top-level directory
  val tlDir = new File("./data").getCanonicalFile
  log("Using data directory: %s" format tlDir.getCanonicalPath)
  
  // find input trials
  val inTrials = InputMarshalling.getTrials(tlDir)
  
}

object Analysis {
  def main(args: Array[String]) {
    val analysis = new Analysis with ConsoleLogger
  }
}