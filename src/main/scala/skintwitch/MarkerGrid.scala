package skintwitch

import scala.collection.immutable._
import mocaputils.Marker
import scalala.tensor.Matrix
import scalala.tensor.dense.DenseMatrix

trait MarkerGrid extends Grid[Marker] { self =>
  
  /** Computes the deformation gradient tensor over the grid of markers.
   * 
   *  At each vertex, every triad of markers is queried to find the deformation
   *  gradient tensor (using the `TensorUtils.dgtri` method), and the mean is
   *  found.
   *  
   *  @see Grid.getConnectedTris
   *  
   *  @param s0 sample representing the "undeformed" state
   *  @param s sample representing the "deformed" state
   *  @param undeformedCoordSys `true` if the tensor should be expressed in the
   *    coordinate system of the `s0` (undeformed) markers, or `false` if the 
   *    tensor should be expressed in the coordinate system of the `s` 
   *    (deformed) markers
   *  @return new grid with the deformation gradient tensor computed at each
   *    vertex */
  def deformationGrad(s0: Int, s: Int, undeformedCoordSys: Boolean = true): 
  Grid[Matrix[Double]] =
    new Grid[Matrix[Double]] {
      val numRows = self.numRows
      val numCols = self.numCols
      def apply(row: Int, col: Int) = {
        val tris = self.getConnectedTriElements(row, col)
        val tensors = tris.map(t => TensorUtils.dgtri(
          (t._1.xs(s0), t._1.ys(s0), t._1.zs(s0)),
          (t._2.xs(s0), t._2.ys(s0), t._2.zs(s0)),
          (t._3.xs(s0), t._3.ys(s0), t._3.zs(s0)),
          (t._1.xs(s),  t._1.ys(s),  t._1.zs(s)),
          (t._2.xs(s),  t._2.ys(s),  t._2.zs(s)),
          (t._3.xs(s),  t._3.ys(s),  t._3.zs(s)),
          undeformedCoordSys
        ))
        TensorUtils.mean(tensors)
      }
    }
  
  /** Computes the gradient of the deformation gradient tensor over the
   *  grid of markers.  A simple linear finite difference approximation is 
   *  used.
   *  
   *  @param sample sample at which to compute the tensor
   *  @return new grid of the computed tensor */
  def deformationGradGrad(sample: Int): Grid[Matrix[Double]] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    val coordSys = if (sample > 0) false else true
    deformationGrad(s0, sample, coordSys).map(_ / period)
  }
  
  /** Computes the Biot strain tensor over the grid of markers.
   *
   *  @param s0 undeformed sample
   *  @param s deformed sample
   *  @param undeformedCoordSys
   *  @return new grid of the Biot strain tensor */
  def biot(s0: Int, s: Int, undeformedCoordSys: Boolean = true):
  Grid[Matrix[Double]] = {
    val fGrid = deformationGrad(s0, s, undeformedCoordSys)
    fGrid.map(e => {
      val (r, u) = TensorUtils.polarDecomp(e)
      u - DenseMatrix.eye[Double](3)
    })
  }
  
  /** Computes the gradient of the Biot strain tensor over the grid of
   *  markers.  A simple linear finite different approximation is used.
   *  
   *  @param sample sample at which to compute the tensor
   *  @return new grid of the computed tensor */
  def biotGrad(sample: Int): Grid[Matrix[Double]] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    val coordSys = if (sample > 0) false else true
    biot(s0, sample, coordSys).map(_ / period)
  }
  
}

object MarkerGrid {

  /** Constructs a `MarkerGrid` from a sequence of specially-named markers.
   *  
   *  Each marker which will form part of the grid should be named as
   *  `CxRy`, where `x` is the column, and `r` is the row.
   *  
   *  @param markers sequence of markers
   *  @return marker grid */
  def fromCRMarkers(markers: Seq[Marker]): MarkerGrid = {
    // find all markers which match the CxRy pattern, and put them into a
    //  sequence of ColRowMarker case classes
    case class ColRowMarker(col: Int, row: Int, marker: Marker)
    val CRTemplate = """C(\d+)R(\d+)""".r
    val crMarkers = for {
      m <- markers
      crmOption = m.name match {
        case CRTemplate(c, r) => Some(ColRowMarker(c.toInt, r.toInt, m))
        case _ => None
      }
      if (crmOption.isDefined)
    } yield crmOption.get
    
    // find min/max col and min/max row
    val cols = crMarkers.map(_.col)
    val rows = crMarkers.map(_.row)
    val (minCol, maxCol) = (cols.min, cols.max)
    val (minRow, maxRow) = (rows.min, rows.max)

    // populate an anonymous grid which looks-up rows and columns
    val lookupGrid = new MarkerGrid {
      val numRows = maxRow - minRow + 1
      val numCols = maxCol - minCol + 1
      def apply(row: Int, col: Int) = crMarkers.
        find(m => m.row == row + minRow && m.col == col + minCol).get.marker
    }
    
    // convert to a wrapped VGrid (for faster access: should be faster lookup)
    new MarkerGrid {
      private val vgrid = VGrid(lookupGrid)
      val numRows = vgrid.numRows
      val numCols = vgrid.numCols
      def apply(row: Int, col: Int) = vgrid(row, col)
      override def map[Z](f: Marker => Z) = vgrid.map(f)
    }
  }

}