package skintwitch.analysis

import java.io.File
import scala.collection.immutable._

class OutputMarshalling

object OutputMarshalling {

  val plotDir = "./output/plots"
  val csvDir = "./output/csv"
  
  def verifyOrCreateOutputDirs() = {
    val plotFileDir = new File(plotDir)
    val csvFileDir = new File(csvDir)
    val dirs: Seq[File] = Seq(plotFileDir, csvFileDir)
    for (dir <- dirs) {
      if (dir.exists) {
        assert(dir.isDirectory)
      } else {
        dir.mkdirs()
      }
    }
  }
    
  def getMaxResponsePlotFileName(trial: Trial.Result): String = {
    "%s/%s-maxresponse.pdf" format (plotDir, trial.idString)
  }

  def getAvgBySiteWithinHorsePlotFileName(horse: String, site: String): 
  String = {
    "%s/%s-avg-%s.pdf" format (plotDir, horse, site)
  }
  
  def getAvgBySiteBetweenHorsePlotFileName(site: String): String = {
    "%s/avg-across-horses-%s.pdf" format (plotDir, site)
  }
  
  def getDistancePlotFileName(trial: Trial.Result): String = {
    "%s/%s-distance.pdf" format (plotDir, trial.idString)
  }
  
  def getI1PlotFileName(trial: Trial.Result): String = {
    "%s/%s-i1.pdf" format (plotDir, trial.idString)
  }
  
  def getPokeParametersFileName(): String = {
    "%s/poke-parameters.csv" format (csvDir)
  }
  
  def getMinPrinStrainsFileName(): String = {
    "%s/min-prin-strains.csv" format (csvDir)
  }
  
  def getAverageGridsFileName(site: String): String = {
    "%s/avg-grid-%s.csv" format (csvDir, site)
  }
  
}
