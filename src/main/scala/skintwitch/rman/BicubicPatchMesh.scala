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
/** 3D vector. */
case class V3(e0: Double, e1: Double, e2: Double) {
  def x(o: V3) = V3(e1 * o.e2 - e2 * o.e1, 
                    e2 * o.e0 - e0 * o.e2,
                    e0 * o.e1 - e1 * o.e0)
  def length = sqrt(e0 * e0 + e1 * e1 + e2 * e2)
  def normalized = V3(e0 / length, e1 / length, e2 / length)
  def -(o: V3) = V3(e0 - o.e0, e1 - o.e1, e2 - o.e2)
  def +(o: V3) = V3(e0 + o.e0, e1 + o.e1, e2 + o.e2)
  def *(c: Double) = V3(e0 * c, e1 * c, e2 * c)
  def lerp(o: V3) = new Lerp[V3] {
    def apply(x: Double) = {
      val y = 1.0 - x
      V3(e0 * y + o.e0 * x, e1 * y + o.e1 * x, e2 * y + o.e2 * x)
    }
  } 
}
/** 4D column vector. */
case class C4(e0: Double, e1: Double, e2: Double, e3: Double)
/** 4D row vector. */
case class R4(e0: Double, e1: Double, e2: Double, e3: Double) {
  def *(c: C4): Double = e0 * c.e0 + e1 * c.e1 + e2 * c.e2 + e3 * c.e3
}
/** 4x4 matrix. */
case class M4(e00: Double, e01: Double, e02: Double, e03: Double,
              e10: Double, e11: Double, e12: Double, e13: Double,
              e20: Double, e21: Double, e22: Double, e23: Double,
              e30: Double, e31: Double, e32: Double, e33: Double)
{
  def *(c: C4) = C4(
    e00 * c.e0 + e01 * c.e1 + e02 * c.e2 + e03 * c.e3,
    e10 * c.e0 + e11 * c.e1 + e12 * c.e2 + e13 * c.e3,
    e20 * c.e0 + e21 * c.e1 + e22 * c.e2 + e23 * c.e3,
    e30 * c.e0 + e31 * c.e1 + e32 * c.e2 + e33 * c.e3
  )
  def *(m: M4) = M4(
    // row 1
    e00 * m.e00 + e01 * m.e10 + e02 * m.e20 + e03 * m.e30,
    e00 * m.e01 + e01 * m.e11 + e02 * m.e21 + e03 * m.e31,
    e00 * m.e02 + e01 * m.e12 + e02 * m.e22 + e03 * m.e32,
    e00 * m.e03 + e01 * m.e13 + e02 * m.e23 + e03 * m.e33,
    // row 2
    e10 * m.e00 + e11 * m.e10 + e12 * m.e20 + e13 * m.e30,
    e10 * m.e01 + e11 * m.e11 + e12 * m.e21 + e13 * m.e31,
    e10 * m.e02 + e11 * m.e12 + e12 * m.e22 + e13 * m.e32,
    e10 * m.e03 + e11 * m.e13 + e12 * m.e23 + e13 * m.e33,
    // row 3
    e20 * m.e00 + e21 * m.e10 + e22 * m.e20 + e23 * m.e30,
    e20 * m.e01 + e21 * m.e11 + e22 * m.e21 + e23 * m.e31,
    e20 * m.e02 + e21 * m.e12 + e22 * m.e22 + e23 * m.e32,
    e20 * m.e03 + e21 * m.e13 + e22 * m.e23 + e23 * m.e33,
    // row 4
    e30 * m.e00 + e31 * m.e10 + e32 * m.e20 + e33 * m.e30,
    e30 * m.e01 + e31 * m.e11 + e32 * m.e21 + e33 * m.e31,
    e30 * m.e02 + e31 * m.e12 + e32 * m.e22 + e33 * m.e32,
    e30 * m.e03 + e31 * m.e13 + e32 * m.e23 + e33 * m.e33
  )
  def *(c: Double) = M4(
    e00 * c, e01 * c, e02 * c, e03 * c,
    e10 * c, e11 * c, e12 * c, e13 * c,
    e20 * c, e21 * c, e22 * c, e23 * c,
    e30 * c, e31 * c, e32 * c, e33 * c
  )
  def t = M4(
    e00, e10, e20, e30,
    e01, e11, e21, e31,
    e02, e12, e22, e32,
    e03, e13, e23, e33
  )
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
