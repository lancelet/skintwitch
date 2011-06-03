package skintwitch

import scala.util.logging.{ ConsoleLogger, Logged }
import mocaputils.TRCReader

class DGTriAnalysis extends Logged {

  // banner
  log("+---------------+")
  log("| DGTriAnalysis |")
  log("+---------------+")
  
  // load an arbitrary file at this stage
  val trcData = TRCReader.read("./data/Horse1_trials30j.trc").fold(
    e => {
      log("Could not read TRC file!")
      sys.exit(-1)
    },
    s => s
  )
  
  // convert trc data to a MarkerGrid
  val markerGrid = MarkerGrid.fromTRC(trcData)
  log("Constructed marker grid.")
  log("numRows = %d" format markerGrid.numRows)
  log("numCols = %d" format markerGrid.numCols)

  // create a DGTensorGrid using the tri method
  val dgrid = DGTensorGrid.triGrid(markerGrid)
  log("Constructed deformation tensor gradient grid")
  
  // export
  val renderer = DGTensorGridRenderer(dgrid)
  for (frame <- 0 until trcData.numFrames) {
    log("computing frame %d of %d" format (frame, trcData.numFrames-1))
    val fname = "./output/tensors/%5d.png" format frame
    renderer.saveToPNG(fname, 50, 0, frame)
  }
  
}

object DGTriAnalysis {
  def main(args: Array[String]) {
    new DGTriAnalysis with ConsoleLogger
  }
}