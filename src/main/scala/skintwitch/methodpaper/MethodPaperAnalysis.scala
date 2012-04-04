package skintwitch.methodpaper

import java.io.File
import skintwitch.analysis.InputMarshalling
import skintwitch.analysis.TrialInput
import java.io.FileWriter

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
  // Specify data output directory
  val outDir = new File("./method-paper-output").getCanonicalFile
  println("Using output directory: %s" format outDir.getCanonicalPath)
  
  //--------------------------------------------------------------------------
  // Find input trials for horse 11.
  val horse = "Horse11"
  println("Using ONLY trials for %s" format horse)
  val inTrials = InputMarshalling.getTrials(dataDir).filter(_.horse == horse)
  
  //--------------------------------------------------------------------------
  // Load all trials for horse 11.
  // This involves mapping each TrialInput object to a MethodPaperTrial object.
  // In doing so, we perform most of the processing required, leaving us free
  // to query the MethodPaperTrial object for information we need.
  println("Processing trials for %s" format horse)
  val trials = (for (inTrial <- inTrials.par) yield {
    val trial = try {
      MethodPaperTrial(inTrial)
    } catch { // CAUTION: INDISCRIMINATE EXCEPTION CATCHING: REPORT ALL BELOW
      case e => {
        println("Exception loading trial %s" format inTrial.inputFile.getName)
        println ("%s" format e)
        throw new Exception(e)
      }
    }
    System.out.println("Loaded trial %s" format inTrial.inputFile.getName)
    System.out.flush
    trial
  }).seq
  println("Loaded %d trials" format trials.length)

  //---------------------------------------------------------------------------
  // Output data for the minimum principal strain at the point of maximum
  // twitch response.
  def saveMinPrinStrains() {
    val outFile = new File(outDir, "min-prin-strains.csv")
    val o = new FileWriter(outFile)
    o.write(
      "Trial," +
      "Strain\n"
    )
    val bySite = trials.sortBy(_.in.site)
    for (trial <- bySite) {
      o.write(trial.in.site + ", " + 
              trial.minPrinStrainAtMaxResponse + "\n")
    }
    o.close()
  }
  saveMinPrinStrains()
  
}