package skintwitch

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Vector
import scala.collection.immutable.VectorBuilder

/** Bicubic interpolator over a grid of Doubles.
  * 
  * This class performs bicubic interpolation over a regular grid.  The apply()
  * method can look up the interpolation at an arbitrary point.  The toGrid()
  * method computes a one-off interpolation (as another Grid) at a particular
  * resolution. 
  * 
  * @param g grid of Doubles to interpolate */
case class BicubicInterpGrid[T : DoubleMultiplyable](g: Grid[T]) { outer =>
  
  private val numRowPatches = g.numRows - 1
  private val numColPatches = g.numCols - 1
  
  private val dmt = implicitly[DoubleMultiplyable[T]]
  private val ge: Grid[T] = g.expandEdgesByOne()(
      () => Linearizable.doubleMultiplyableToLinearizable[T])
  private case class GridSubSamp(r: Int, c: Int) extends IndexedSeq[T] {
    val length: Int = 16
    def apply(idx: Int): T = ge(r + (idx / 4), c + (idx % 4))
  }
  
  private val patches: IndexedSeq[BicubicPatch[T]] = {
    val vb = new VectorBuilder[BicubicPatch[T]]()
    vb.sizeHint(numRowPatches * numColPatches) 
    for {
      r <- 0 until numRowPatches
      c <- 0 until numColPatches
    } {
      vb += BicubicPatch(GridSubSamp(r, c))
    }
    vb.result()
  }
  private def getPatch(row: Int, col: Int) = {
    require(row >= 0 && row < numRowPatches, 
            "row %d out of range" format row)
    require(col >= 0 && col < numColPatches,
            "col %d out of range" format col)
    col + row * (g.numCols - 1)
  }

  def apply(u: Double, v: Double): T = {
    require(u >= 0.0 && u <= 1.0)
    require(v >= 0.0 && v <= 1.0)
    
    // find which patch and patch coords
    val ue = u * numColPatches
    val ve = v * numRowPatches
    val patchCol = {
      val p = ue.toInt
      if (p < numColPatches) p else numColPatches - 1
    }
    val patchRow = {
      val p = ve.toInt
      if (p < numRowPatches) p else numRowPatches - 1
    }
    val uu = ue % 1.0
    val vv = ve % 1.0
        
    // look-up in a single patch
    patches(getPatch(patchRow, patchCol))(uu, vv)
  }
  
  def toGrid(nRows: Int, nCols: Int): Grid[T] = VGrid[T](
    new Grid[T] {
      val numRows = nRows
      val numCols = nCols
      private val nRowPatches = (numRows - 1).toDouble
      private val nColPatches = (numCols - 1).toDouble
      def apply(row: Int, col: Int): T =
        outer(row / nRowPatches, col / nColPatches)
    }
  )
  
}
