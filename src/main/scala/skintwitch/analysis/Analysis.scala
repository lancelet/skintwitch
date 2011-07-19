package skintwitch.analysis

import scala.collection.immutable._
import java.io.File
import skintwitch.renderplanar.Render2DTensors
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
  // find input trials
  val inTrials = InputMarshalling.getTrials(tlDir)
  
  //--------------------------------------------------------------------------
  // map each input trial to a Trial object.  this does most of the
  // processing required, leaving us to query the Trial object for the plot
  // quantities we need.
  println("Loading trials")
  val trials = (for (inTrial <- inTrials.par) yield {
    val trial = try {
      Trial(inTrial)
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
      "Poke X," +
      "Poke Y," +
      "Peak I1 X," +
      "Peak I1 Y\n")
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
          val peakI1 = trial.peakI1AtMaximumResponse
          val pokeI1 = if (trial.i1AtPokeLocation.isDefined) {
            "%f" format trial.i1AtPokeLocation.get
          } else {
            ""
          }
          val (pokeX, pokeY) = if (trial.pokeGridLocation.isDefined) {
            ("%f" format trial.pokeGridLocation.get._1,
             "%f" format trial.pokeGridLocation.get._2)
          } else { 
            ("", "") 
          }
          val peakI1X = trial.peakI1GridLocation._1
          val peakI1Y = trial.peakI1GridLocation._2
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
                                Seq(trial.pokeLocation),
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
    val pokes = trials.map(_.pokeLocation)
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
      avgPokeLocationForHorse = Averaging.mean(horseTrials.map(_.pokeLocation))
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
  
}

object Analysis {
  def main(args: Array[String]) {
    val analysis = new Analysis
  }
}