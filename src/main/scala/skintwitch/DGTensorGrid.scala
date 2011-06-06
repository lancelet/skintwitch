package skintwitch

trait DGTensorGrid {
  
  val numRows: Int
  val numCols: Int
  
  val markerGrid: MarkerGrid
  
  def apply(i: Int, j: Int, s0: Int, s1: Int): DGTensor
  
}

object DGTensorGrid {
  
  def triGrid(g: MarkerGrid): DGTensorGrid = {
    new DGTensorGrid {
      val numRows = g.numRows
      val numCols = g.numCols
      val markerGrid = g
      def apply(i: Int, j: Int, s0: Int, s1: Int): DGTensor = {
        require(i >= 0 && i < numCols)
        require(j >= 0 && j < numRows)
        val triSeq = for ((ma, mb, mc) <- g.getMarkerTris(i, j)) yield {
          DGTri.dgtri(
            ma.co(s0), mb.co(s0), mc.co(s0),
            ma.co(s1), mb.co(s1), mc.co(s1)
          )
        }
        assert(triSeq.length > 0)
        DGTensor.mean(triSeq)
      }
    }
  }
  
}