package skintwitch.analysis

class OutputMarshalling

object OutputMarshalling {

  val plotDir = "./output/plots"
  
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
  
}
