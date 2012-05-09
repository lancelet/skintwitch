package skintwitch.analysis

import scala.collection.immutable._
import java.io.File
import skintwitch.renderplanar.Render2DTensors
import skintwitch.Vec2
import java.io.FileWriter

class Analysis {

  //--------------------------------------------------------------------------
  // banner
  println("+-------------------------+")
  println("| Analysis of Skin Motion |")
  println("+-------------------------+")
  
  //--------------------------------------------------------------------------
  // top-level directory
  val tlDir = new File("./data").getCanonicalFile
  println("Using data directory: %s" format tlDir.getCanonicalPath)
  
  //--------------------------------------------------------------------------
  // verify / create output directories
  println("Verifying / creating output directories")
  OutputMarshalling.verifyOrCreateOutputDirs()
  
  //--------------------------------------------------------------------------
  // find input trials
  val inTrials = InputMarshalling.getTrials(tlDir)
  
  //--------------------------------------------------------------------------
  // map each input trial to a Trial object.  this does most of the
  // processing required, leaving us to query the Trial object for the plot
  // quantities we need.
  println("Loading trials")
  val trials: Seq[Trial.Result] = (for (inTrial <- inTrials.par) yield {
    val trial = try {
      // this should create Trial as a temporary object; perform processing
      //  and then dump the Trial, leaving only the TrialResult (making memory
      //  use much less).
      Trial(inTrial).result
    } catch { // THIS IS BAD: DO NOT CATCH ALL EXCEPTIONS INDISCRIMINATELY
      case e => {
        println("Exception loading trial %s" format inTrial.inputFile.getName)
        println("%s" format e)
        throw new Exception(e)
      }
    }
    System.out.println("Loaded trial %s" format inTrial.inputFile.getName)
    System.out.flush
    trial
  }).seq
  println("Loaded %d trials." format trials.length)
  
  //--------------------------------------------------------------------------
  // plot distance of the poker from the marker grid vs time
  println("Plotting poker distance from grid vs time")
  for (trial <- trials.par) {
    val fileName = OutputMarshalling.getDistancePlotFileName(trial)
    DistancePlot.plot(fileName, trial)
  }
  println("Done")
  
  //--------------------------------------------------------------------------
  // plot average I1 value over the grid vs time.  I1 is the First Invariant
  // of the Left Cauchy-Green Deformation Tensor, equal to the sums of the
  // squares of the principal stretches.
  println("Plotting average I1 over the grid vs time")
  for (trial <- trials.par) {
    val fileName = OutputMarshalling.getI1PlotFileName(trial)
    I1Plot.plot(fileName, trial)
  }
  println("Done")
  
  //--------------------------------------------------------------------------
  // write a CSV file summarising the poke statistics for all trials
  println("Saving poke parameters for statistical analysis")
  def saveCSV() { // function to avoid polluting outer scope
    val pokeParametersFile = OutputMarshalling.getPokeParametersFileName()
    val o = new FileWriter(pokeParametersFile)
    o.write(
      "Horse," +
      "Trial Number," +
      "Site," +
      "Distance from poke to peak I1 at maximum response (mm)," +
      "Peak I1 value at maximum response (stretch squared)," +
      "I1 value at the poke location at maximum response (stretch squared)," +
      "Poke X (\\xi^1)," +
      "Poke Y (\\xi^2)," +
      "Peak I1 X (\\xi^1)," +
      "Peak I1 Y (\\xi^2)\n")
    val byHorse = trials.groupBy(_.in.horse).toSeq.sortBy(_._1).map(_._2)
    for (horseTrials <- byHorse) {
      val bySite = horseTrials.groupBy(_.in.site).toSeq.sortBy(_._1).map(_._2)
      for (siteTrials <- bySite) {
        val byNumber = siteTrials.sortBy(_.in.trialNumber)
        for (trial <- byNumber) {
          val horse = trial.in.horse
          val num = trial.in.trialNumber
          val site = trial.in.site
          val distPokeI1 = if (trial.distanceFromPokeToMaxI1.isDefined) {
            "%f" format trial.distanceFromPokeToMaxI1.get
          } else {
            ""
          }
          val peakI1 = trial.i1_peak
          val pokeI1 = if (trial.i1AtPokeLocation.isDefined) {
            "%f" format trial.i1AtPokeLocation.get
          } else {
            ""
          }
          val (pokeX, pokeY) = if (trial.pokeMeshDistance.isDefined) {
            ("%f" format trial.pokeMeshDistance.get.st.x,
             "%f" format trial.pokeMeshDistance.get.st.y)
          } else { 
            ("", "") 
          }
          val peakI1X = trial.i1_peak_st.x
          val peakI1Y = trial.i1_peak_st.y
          o.write("%s,%d,%s,%s,%f,%s,%s,%s,%f,%f\n" 
                  format (horse, num, site, distPokeI1, peakI1, pokeI1,
                          pokeX, pokeY, peakI1X, peakI1Y))
        }
      }
    }
    o.close
  }
  saveCSV()
  println("Done")
  
  //--------------------------------------------------------------------------
  // create maximum response plots for each trial.  the maximum response plot
  // is the plot of the Biot strain tensor at the time of the first peak in the 
  // first invariant of the Left Cauchy-Green deformation tensor.
  println("Creating maximum response plots for each trial")
  for (trial <- trials) {
    val plotFileName = OutputMarshalling.getMaxResponsePlotFileName(trial)
    val strokePath = if (trial.in.site == "Girthline") {
      Some(Seq(trial.strokePath.get))
    } else {
      None
    }
    val renderContours = (trial.in.site != "Control")
    Render2DTensors.renderToPDF(plotFileName, Seq(trial.biot2d), 
                                Seq(trial.pokeMeshDistance.map(_.st)),
                                strokePath, renderContours)
  }
  println("Done")
  
  //--------------------------------------------------------------------------
  // create average maximum response plots for each site within a horse
  println("Creating average max response plots for each site within a horse")
  for { 
    horseTrials <- trials.groupBy(_.in.horse).map(_._2)
    siteTrials <- horseTrials.groupBy(_.in.site)
  } {
    val horse = siteTrials._2.head.in.horse
    val site = siteTrials._1
    val trials = siteTrials._2
    val grids = trials.map(_.biot2d)
    val pokes = trials.map(_.pokeMeshDistance.map(_.st))
    val plotFileName = OutputMarshalling.getAvgBySiteWithinHorsePlotFileName(
      horse, site)
    val strokePaths = if (site == "Girthline") {
      Some(trials.map(_.strokePath.get))
    } else {
      None
    }
    val renderContours = (site != "Control")
    Render2DTensors.renderToPDF(plotFileName, grids, pokes, strokePaths,
                                renderContours)
  }
  println("Done")
 
  //--------------------------------------------------------------------------
  // create average maximum response plots for each site among all horses
  println("Creating average max response plots for each site between horses")
  for {
    siteTrials <- trials.groupBy(_.in.site).map(_._2)
  } {
    val (grids, pokes) = (for {
      horseTrials <- siteTrials.groupBy(_.in.horse).map(_._2)
      avgGridForHorse = Averaging.mean(horseTrials.map(_.biot2d))
      avgPokeLocationForHorse = Averaging.mean(horseTrials.map(
          _.pokeMeshDistance.map(_.st)))
    } yield (avgGridForHorse, avgPokeLocationForHorse)).unzip
    val site = siteTrials.head.in.site
    val plotFileName = OutputMarshalling.
      getAvgBySiteBetweenHorsePlotFileName(site)
    val strokePaths = if (site == "Girthline") {
      Some(siteTrials.map(_.strokePath.get))
    } else {
      None
    }
    val renderContours = (site != "Control")
    Render2DTensors.renderToPDF(plotFileName, grids.toIndexedSeq, 
                                pokes.toIndexedSeq, strokePaths,
                                renderContours)
  }
  println("Done")
  
  //---------------------------------------------------------------------------
  // Output data for the minimum principal strain (compressive) at the point of
  // maximum twitch response.  (For each horse, we normalize the strains by
  // dividing through by the average strain for each horse.)
  // Try without normalizing strains.
  def saveMinPrinStrains() {
    val outFile = new File(OutputMarshalling.getMinPrinStrainsFileName())
    val o = new FileWriter(outFile)
    
    o.write("Site,Horse,Strain\n") // header line

    for {
      trial <- trials
      site = trial.in.site
      horse = trial.in.horse
      strain = trial.minPrinStrainAtMaxResponse
    } {
      o.write("%s,%s,%f\n" format(site, horse, strain))
    }
    
    o.close()
  }
  println("Saving minimum principal strains at max response")
  saveMinPrinStrains()
  println("Done")
  
  //---------------------------------------------------------------------------
  // Output average grids of I1, Biot strain and poke location lists for each
  // test site, across all horses.  This is substantially the same code as
  // appears in MethodPaperAnalysis.scala.
  def saveAverageGrids() {
    val sites = trials.map(_.in.site).toSet.toList.sorted
    for {
      site <- sites
      trialsAtSite = trials.filter(_.in.site == site)
    } {
      val pokeLocs = trialsAtSite.map(_.pokeMeshDistance.map(_.st))
      val siteBiot2d = Averaging.mean(trialsAtSite.map(_.biot2d))
      val siteI1 = Averaging.meanGridDouble(trialsAtSite.map(_.i1Grid_peak))
      val outFile = new File(OutputMarshalling.getAverageGridsFileName(site))
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
  println("Saving average grids at max response")
  saveAverageGrids()
  println("Done")
  
  //---------------------------------------------------------------------------
  // Output maximum response locations, adjusted so that they're expressed 
  // relative to the poke location of each trial.
  def saveRelativeResponseLocations() {
    val outFile = 
      new File(OutputMarshalling.getRelativeResponseLocationsFileName)
    val o = new FileWriter(outFile)
    o.write("Site,Horse,RelX,RelY\n")
    for (trial <- trials) {
      val site: String = trial.in.site
      val horse: String = trial.in.horse
      val poke_st_opt: Option[Vec2] = trial.pokeMeshDistance.map(_.st)
      val i1_peak_st: Vec2 = trial.i1_peak_st
      if (poke_st_opt.isDefined) {
        val poke_st = poke_st_opt.get
        val rel_xy = i1_peak_st - poke_st
        o.write("%s,%s,%s,%s\n" format(site, horse, rel_xy.x, rel_xy.y))
      }
    }
    o.close()
  }
  println("Saving maximum response locations relative to pokes")
  saveRelativeResponseLocations()
  println("Done")
  
}

object Analysis {
  def main(args: Array[String]) {
    val analysis = new Analysis
  }
}