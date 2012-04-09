package skintwitch

import scala.collection.immutable.IndexedSeq

/** Bicubic patch.
  * 
  * This patch does bicubic interpolation over a 4x4 regularly-spaced grid of
  * any Fractional type which can be multiplied by a Double 
  * (DoubleMultiplyable).  The grid should be passed in as parameter 'v',
  * expressed in row-major form.  Interpolation is performed using the
  * apply() function.  Interpolation is done first in the row direction and
  * then in the column direction.  Spline parameters in 'x' are cached, but
  * must necessarily be re-evaluated for each 'y' value.
  * 
  * @param v 16-element Double IndexedSeq, containing a grid of a
  *   DoubleMultiplyable type expressed in row-major form */
case class BicubicPatch[T : DoubleMultiplyable](v: IndexedSeq[T]) {
  require(v.length == 16, "requires an IndexedSeq with exactly 16 elements")
  
  private val tMul = implicitly[DoubleMultiplyable[T]]
  import tMul._
  private val two = fromInt(2)
  private val three = fromInt(3)
  
  private case class Cubic(c0: T, c1: T, c2: T, c3: T) {
    private val f1p = (c3 - c1) / two
    private val f0p = (c2 - c0) / two
    private val a = f1p - two * c2 + f0p + two * c1
    private val b = three * c2 - f1p - two * f0p - three * c1
    private val c = f0p
    private val d = c1
    def apply(x: Double): T = {
      require(x >= 0.0 && x <= 1.0)
      // a * x^3 + b * x^2 + c * x + d
      val r1 = multiplyDouble(a, x) + b
      val r2 = multiplyDouble(r1, x) + c
      multiplyDouble(r2, x) + d
    }
  }
  
  private val cx0 = Cubic(v(0), v(1), v(2), v(3))
  private val cx1 = Cubic(v(4), v(5), v(6), v(7))
  private val cx2 = Cubic(v(8), v(9), v(10), v(11))
  private val cx3 = Cubic(v(12), v(13), v(14), v(15))

  def apply(u: Double, v: Double): T = {
    require(u >= 0.0 && u <= 1.0, "u must be in [0.0, 1.0]")
    require(v >= 0.0 && v <= 1.0, "v must be in [0.0, 1.0]")
    Cubic(cx0(u), cx1(u), cx2(u), cx3(u))(v)
  }
}
