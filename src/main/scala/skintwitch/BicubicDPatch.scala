package skintwitch

import scala.collection.immutable.IndexedSeq

/** Bicubic patch (4x4) for use on Doubles.
  *
  * This patch does bicubic interpolation over a 4x4 regularly-spaced grid of 
  * Doubles.  The grid should be passed in as parameter v, expressed in 
  * row-major form.  Interpolation is performed using the apply() function.  
  * Interpolation is done first in the row direction and then in the column 
  * direction.  Spline parameters in x are cached, but must necessarily be 
  * re-evaluated for each y value.
  * 
  * @param v 16-element Double IndexedSeq, containing a grid of Doubles
  *   expressed in row-major form */
case class BicubicDPatch(v: IndexedSeq[Double]) {
  require(v.length == 16)  
  
  private case class Cubic(c0: Double, c1: Double, c2: Double, c3: Double) {
    private val f1p = (c3 - c1) / 2.0
    private val f0p = (c2 - c0) / 2.0
    private val a = f1p - 2 * c2 + f0p + 2 * c1
    private val b = 3 * c2 - f1p - 2 * f0p - 3 * c1
    private val c = f0p
    private val d = c1
    def apply(x: Double): Double = {
      require(x >= 0.0 && x <= 1.0)
      (((a * x + b) * x) + c) * x + d
    }
  }

  private val cx0 = Cubic(v(0), v(1), v(2), v(3))
  private val cx1 = Cubic(v(4), v(5), v(6), v(7))
  private val cx2 = Cubic(v(8), v(9), v(10), v(11))
  private val cx3 = Cubic(v(12), v(13), v(14), v(15))
  
  def apply(u: Double, v: Double): Double = {
    require(u >= 0.0 && u <= 1.0)
    require(v >= 0.0 && v <= 1.0)
    Cubic(cx0(u), cx1(u), cx2(u), cx3(u))(v)
  }  
}
