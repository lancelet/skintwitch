package skintwitch

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Vector
import scala.collection.immutable.VectorBuilder
import scala.math.floor

/** Bicubic interpolator over a grid of Doubles.
  * 
  * This class performs bicubic interpolation over a regular grid.  The apply()
  * method can look up the interpolation at an arbitrary point.  The toGrid()
  * method computes a one-off interpolation (as another Grid) at a particular
  * resolution. 
  * 
  * @param g grid of Doubles to interpolate */
case class BicubicInterpGrid(g: Grid[Double]) { outer =>
  
  private val ge: Grid[Double] = g.expandEdgesByOne()
  private case class GridSubSamp(i: Int, j: Int) extends IndexedSeq[Double] {
    val length: Int = 16
    def apply(idx: Int): Double = ge(i + (idx % 4), j + (idx / 4))
  }
  
  private val patches: IndexedSeq[BicubicDPatch] = {
    val vb = new VectorBuilder[BicubicDPatch]()
    vb.sizeHint(g.numCols * g.numRows) 
    for {
      j <- 0 until (g.numCols - 1)
      i <- 0 until (g.numRows - 1)
    } {
      vb += BicubicDPatch(GridSubSamp(i, j))
    }
    vb.result()
  }

  def apply(u: Double, v: Double): Double = {
    // find out which patch we're in, and the individual patch coords
    val ue = u * (g.numCols - 1)
    val ve = v * (g.numRows - 1)
    val patchx = floor(ue).toInt
    val patchy = floor(ve).toInt
    val uu = ue - patchx
    val vv = ve - patchy
    
    // look-up in a single patch
    val patchIdx = patchx + patchy * g.numCols
    patches(patchIdx)(uu, vv)
  }
  
  def toGrid(nRows: Int, nCols: Int): Grid[Double] = VGrid[Double](
    new Grid[Double] {
      val numRows = nRows
      val numCols = nCols
      def apply(row: Int, col: Int): Double = {
        val u = row / (numRows - 1).toDouble
        val v = col / (numCols - 1).toDouble
        outer(u, v)
      }
    }
  )
  
}
