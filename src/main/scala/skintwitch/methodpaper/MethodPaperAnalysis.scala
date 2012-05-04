package skintwitch.methodpaper

import java.io.File
import skintwitch.analysis.Averaging
import skintwitch.analysis.InputMarshalling
import skintwitch.analysis.TrialInput
import java.io.FileWriter
import skintwitch.GridDumper

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
  // Find input trials for horse 7.
  val horse = "Horse7"
  println("Using ONLY trials for %s" format horse)
  val inTrials = InputMarshalling.getTrials(dataDir).filter(_.horse == horse)
    //.filter(_.trialNumber == 17)  // un-comment to process just 1 sample trial
  
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
  // Dump out plots of i1 at maximum response
  //for (trial <- trials) {
  //  val outfile = new File(outDir, "%s-%s-%d.png" format (trial.in.horse, 
  //      trial.in.site, trial.in.trialNumber))
  //  GridDumper.dump(outfile, trial.i1AtMaxResponse)
  //}
  
  //---------------------------------------------------------------------------
  // Order trials by site.
  val bySite = trials.sortBy(_.in.site)
  
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
    for (trial <- bySite) {
      o.write(trial.in.site + ", " + 
              trial.minPrinStrainAtMaxResponse + "\n")
    }
    o.close()
  }
  saveMinPrinStrains()
  
  //---------------------------------------------------------------------------
  // Output data for the maximum I1 coordinates at the point of maximum twitch
  // response.
  def saveMaxI1Coords() {
    val outFile = new File(outDir, "max-I1-coords.csv")
    val o = new FileWriter(outFile)
    o.write(
      "Trial," +
      "V (col / x)," +
      "U (row / y)," +
      "I_1(U, V) - 3," +
      "\n"
    )
    for (trial <- bySite) {
      val uv = trial.maxI1AtMaxResponseUVCoords
      val i_uv = trial.i1AtMaxResponseInterp.rowMajor.max - 3.0
      o.write("%s, %f, %f, %f\n" format (trial.in.site, uv.y, uv.x, i_uv))
    }
    o.close()
  }
  saveMaxI1Coords()
  
  //---------------------------------------------------------------------------
  // Output average grids of I1 and Biot strain for each test site.
  def saveAverageGrids() {
    val sites = trials.map(_.in.site).toSet.toList.sorted
    for {
      site <- sites
      trialsAtSite = trials.filter(_.in.site == site)
    } {
      val pokeLocs = trialsAtSite.map(_.pokeLocation)
      val siteBiot2d = Averaging.mean(trialsAtSite.map(_.biot2d))
      val siteI1 = Averaging.meanGridDouble(trialsAtSite.map(_.i1Grid))
      val outFile = new File(outDir, "avg-grid-%s.csv" format site)
      val o = new FileWriter(outFile)
      
      // export I1 grid
      o.write("I1 grid (7 rows, 8 columns)\n")
      for {
        row <- 0 until siteI1.numRows
        col <- 0 until siteI1.numCols 
      } {
        o.write("%f" format siteI1(row, col))
        o.write(if (col < siteI1.numCols - 1) ", " else "\n")
      }
      
      // export Biot strain (E) grid
      o.write("Biot strain (E) (7 rows, 8 columns) ")
      o.write("(v1x, v1y, v2x, v2y, lambda1, lambda2) ")
      o.write("where v1 and v1 are principal strains directions and ")
      o.write("lambda are the magnitudes.\n")
      for {
        row <- 0 until siteBiot2d.numRows
        col <- 0 until siteBiot2d.numCols
      } {
        val eigs = siteBiot2d(row, col).eig
        assert(eigs.length == 2)
        val l1 = eigs(0)._1
        val l2 = eigs(1)._1
        val v1 = eigs(0)._2
        val v2 = eigs(1)._2
        o.write("(%f;%f;%f;%f;%f;%f)" format (v1.x, v1.y, v2.x, v2.y, l1, l2))
        o.write(if (col < siteBiot2d.numCols - 1) ", " else "\n")
      }
      
      // export stimulus (poke) locations
      o.write("Poke locations (u,v).  u is column-linked, v is row-linked.\n")
      for (poke <- pokeLocs) {
        if (poke.isDefined) {
          o.write("%f, %f\n" format(poke.get.x, poke.get.y))
        }
      }
      
      o.close()
    }
  }
  saveAverageGrids()
  
}
