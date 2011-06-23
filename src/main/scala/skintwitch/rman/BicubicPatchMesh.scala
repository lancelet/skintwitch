package skintwitch.rman

import skintwitch.Grid
import math.{ sqrt }
import scala.collection.immutable.IndexedSeq

/** Linear interpolator trait. */
trait Lerp[T] { def apply(x: Double): T }
/** PatchMesh result. */
case class PMResult(p: V3, dpdu: V3, dpdv: V3) {
  import BicubicPatchMesh._
  lazy val n: V3 = (dpdv x dpdu).normalized
  def lerp(o: PMResult) = new Lerp[PMResult] {
    def apply(x: Double) = PMResult(
      (p lerp o.p)(x), (dpdu lerp o.dpdu)(x), (dpdv lerp o.dpdv)(x))
  }
}

/** A RenderMan-style non-periodic bicubic patch mesh.  The patch mesh can
 *  evaluate position, as well as incremental vectors along u and v
 *  parameter directions. */
case class BicubicPatchMesh(ubasis: M4,
                            ustep: Int,
                            vbasis: M4,
                            vstep: Int,
                            nu: Int, nv: Int,
                            p: IndexedSeq[Double]) 
{
  import BicubicPatchMesh._
  require(nu >= 4 && nv >= 4)
  require(p.length == nu * nv * 3)
  private val nupatches = ((nu - 4) / ustep) + 1
  private val nvpatches = ((nv - 4) / vstep) + 1
  private val upatchsize = 1.0 / nupatches
  private val vpatchsize = 1.0 / nvpatches

  // pro-actively split patches, constructing appropriate x, y and z matrices.
  //  this saves us from having to construct these matrices for each patch we
  //  encounter as the entire patch mesh is rapidly evaluated.  patchMats is
  //  indexed first by the u patch number and then by the v patch number.
  private val patchMats: IndexedSeq[IndexedSeq[(M4, M4, M4)]] = 
    (for (uPatch <- 0 until nupatches) yield {
      (for (vPatch <- 0 until nvpatches) yield {
        val grid = new Grid[(Double, Double, Double)] {
          val numRows = 4
          val numCols = 4
          private val ux0 = uPatch * ustep
          private val vx0 = vPatch * vstep
          private def getPIndexed(ux: Int, vx: Int): (Double, Double, Double) = 
          {
            val i = (ux + vx * nu) * 3
            (p(i), p(i+1), p(i+2))
          }
          def apply(row: Int, col: Int) = getPIndexed(ux0 + row, vx0 + col)
        }
        def gridMat(g: Grid[(Double)]) = M4(
          g(0,0), g(0,1), g(0,2), g(0,3),
          g(1,0), g(1,1), g(1,2), g(1,3),
          g(2,0), g(2,1), g(2,2), g(2,3),
          g(3,0), g(3,1), g(3,2), g(3,3))
        val xm = ubasis * gridMat(grid.map(_._1)) * vbasis.t
        val ym = ubasis * gridMat(grid.map(_._2)) * vbasis.t
        val zm = ubasis * gridMat(grid.map(_._3)) * vbasis.t
        (xm, ym, zm)
      }).toIndexedSeq
    }).toIndexedSeq
  
  def apply(u: Double, v: Double, du: Double = 0.01, dv: Double = 0.01): 
  PMResult = {
    require(u >= 0.0 && u <= 1.0)
    require(v >= 0.0 && v <= 1.0)
    
    // find out which patch (u,v) references, and normalize coordinates
    val uPatch = clamp((u / upatchsize).toInt, 0, nupatches - 1)
    val vPatch = clamp((v / vpatchsize).toInt, 0, nvpatches - 1)
    val uu = u / upatchsize - uPatch
    val vv = v / vpatchsize - vPatch
        
    // look up x y and z matrices for the referenced patch
    val (xm, ym, zm) = patchMats(uPatch)(vPatch)

    // evaluates specific uc and vc coordinates within a patch
    def eval(uc: Double, vc: Double): V3 = {
      val um = R4(uc * uc * uc, uc * uc, uc, 1.0)
      val vm = C4(vc * vc * vc, vc * vc, vc, 1.0)
      V3(um * (xm * vm), um * (ym * vm),  um * (zm * vm))
    }

    // evaluate the point, a vector along u, and a vector along v
    val pp = eval(uu, vv)  // point  
    val dpdu = eval(uu + du, vv) - pp  // vector along u
    val dpdv = eval(uu, vv + dv) - pp  // vector along v
    PMResult(pp, dpdu, dpdv)
  }
}

object BicubicPatchMesh {
  /** Clamps `x` between `min` and `max` inclusive. */
  def clamp[T](x: T, min: T, max: T)(implicit q: T => Ordered[T]) = {
    if (x < min) min
    else if (x > max) max
    else x
  }
  /** Catmull-Rom Basis Matrix. */
  val catmullRom = M4(
    -1,  3, -3,  1,
     2, -5,  4, -1,
    -1,  0,  1,  0,
     0,  2,  0,  0
  ) * 0.5
}
