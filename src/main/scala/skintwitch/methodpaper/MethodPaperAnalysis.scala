package skintwitch.methodpaper

import java.io.File
import skintwitch.analysis.InputMarshalling
import skintwitch.analysis.TrialInput

object MethodPaperAnalysis extends App {

  //--------------------------------------------------------------------------
  // Print banner to console
  println("+-------------------------------------------+")
  println("| Analysis of data specific to method paper |")
  println("+-------------------------------------------+")
  
  //--------------------------------------------------------------------------
  // Specify the top-level data directory
  val dataDir = new File("./data").getCanonicalFile
  println("Using data directory: %s" format dataDir.getCanonicalPath)
  
  //--------------------------------------------------------------------------
  // Find input trials for horse 11.
  val horse = "Horse11"
  println("Using ONLY trials for %s" format horse)
  val inTrials = InputMarshalling.getTrials(dataDir).filter(_.horse == horse)
  
}