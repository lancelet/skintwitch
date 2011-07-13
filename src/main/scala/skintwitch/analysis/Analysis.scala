package skintwitch.analysis

import scala.collection.immutable._
import java.io.File
import skintwitch.renderplanar.Render2DTensors

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
  println("Creating average response plots for each site within a horse")
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
  println("Creating average response plots for each site between horses")
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