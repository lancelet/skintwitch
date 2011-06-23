package skintwitch.rman

import scala.collection.immutable.IndexedSeq
import skintwitch.{ Grid, VGrid }
import BicubicPatchMesh.clamp

/** Approximates a bicubic patch mesh using a regular mesh that is tesselated
 *  in advance.  The bicubic patch mesh effectively becomes a bilinear
 *  patch mesh. */
case class ApproxBicubicPatchMesh(ubasis: M4,
                                  ustep: Int,
                                  vbasis: M4,
                                  vstep: Int,
                                  nu: Int, nv: Int,
                                  p: IndexedSeq[Double],
                                  nuTessel: Int = 100,
                                  nvTessel: Int = 100)
{ 
  private val uMeshSize = 1.0 / nuTessel
  private val vMeshSize = 1.0 / nvTessel
    
  // create a real bicubic patch mesh, and tesselate it, creating a grid of
  //  PMResult objects.  creating a VGrid object ensures full evaluation of
  //  the grid and allows the original BicubicPatchMesh to pass out of scope.
  private val tesselation: VGrid[PMResult] = {
    val tgrid = new Grid[PMResult] {
      private val bpm = BicubicPatchMesh(ubasis, ustep, vbasis, vstep, 
                                         nu, nv, p)
      val numRows = nuTessel + 1
      val numCols = nvTessel + 1
      def apply(row: Int, col: Int) = {
        val u = row.toDouble / nuTessel
        val v = col.toDouble / nvTessel
        bpm(u, v)
      }
    }
    VGrid(tgrid)
  }
    
  /** Return approximation of the bicubic patch mesh by bilinearly
   *  interpolating within the tesselated mesh. */
  def apply(u: Double, v: Double): PMResult = {
    require(u >= 0 && u <= 1 && v >= 0 && v <= 1)
    
    // figure out which grid square we're in, and normalize coordinates
    val uGrid = clamp((u / uMeshSize).toInt, 0, nuTessel - 1)
    val vGrid = clamp((v / vMeshSize).toInt, 0, nvTessel - 1)
    val uu = u / uMeshSize - uGrid
    val vv = v / vMeshSize - vGrid

    // do the bilinear interpolation
    val u0v0 = tesselation(uGrid, vGrid)
    val u1v0 = tesselation(uGrid + 1, vGrid)
    val u0v1 = tesselation(uGrid, vGrid + 1)
    val u1v1 = tesselation(uGrid + 1, vGrid + 1)
    ((u0v0 lerp u1v0)(uu) lerp (u0v1 lerp u1v1)(uu))(vv) 
  }
  
}
