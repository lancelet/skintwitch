package skintwitch.analysis

class OutputMarshalling

object OutputMarshalling {

  val plotDir = "./output/plots"
  val csvDir = "./output/csv"
    
  def getMaxResponsePlotFileName(trial: Trial): String = {
    "%s/%s-maxresponse.pdf" format (plotDir, trial.idString)
  }

  def getAvgBySiteWithinHorsePlotFileName(horse: String, site: String): 
  String = {
    "%s/%s-avg-%s.pdf" format (plotDir, horse, site)
  }
  
  def getAvgBySiteBetweenHorsePlotFileName(site: String): String = {
    "%s/avg-across-horses-%s.pdf" format (plotDir, site)
  }
  
  def getDistancePlotFileName(trial: Trial): String = {
    "%s/%s-distance.pdf" format (plotDir, trial.idString)
  }
  
  def getI1PlotFileName(trial: Trial): String = {
    "%s/%s-i1.pdf" format (plotDir, trial.idString)
  }
  
  def getPokeParametersFileName(): String = {
    "%s/poke-parameters.csv" format (csvDir)
  }
  
}