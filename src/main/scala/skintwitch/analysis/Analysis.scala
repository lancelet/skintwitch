package skintwitch.analysis

import java.io.File

class Analysis {

  // banner
  println("+-------------------------+")
  println("| Analysis of Skin Motion |")
  println("+-------------------------+")
  
  // top-level directory
  val tlDir = new File("./data").getCanonicalFile
  println("Using data directory: %s" format tlDir.getCanonicalPath)
  
  // find input trials
  val inTrials = InputMarshalling.getTrials(tlDir)
  
  // map each input trial to a Trial object
  val trials = for (inTrial <- inTrials) yield {
    val trial = Trial(inTrial)
    System.out.println("Loaded trial %s." format inTrial.inputFile.getName)
    System.out.flush
    trial
  }
    
}

object Analysis {
  def main(args: Array[String]) {
    val analysis = new Analysis
  }
}