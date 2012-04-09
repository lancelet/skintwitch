package skintwitch

/** Linearizable type, which can be interpolated and extrapolated using a
 *  single parameter. */
trait Linearizable[T] {
  /** Interpolates / extrapolates between `x0` and `x1` using the equation 
   *  `x0 * (1-s) + x1 * s`. */
  def lin(x0: T, x1: T, s: Double): T
}

object Linearizable {
  class DoubleMultiplyableIsLinearizable[T : DoubleMultiplyable]
  extends Linearizable[T] {
    val d = implicitly[DoubleMultiplyable[T]]
    import d._
    def lin(x0: T, x1: T, s: Double): T = {
      val r0 = multiplyDouble(x0, 1.0 - s)
      val r1 = multiplyDouble(x1, s)
      r0 + r1
    }
  }
  implicit def doubleMultiplyableToLinearizable[T : DoubleMultiplyable] =
    new DoubleMultiplyableIsLinearizable[T]
  /*
  class DoubleIsLinearizable extends Linearizable[Double] {
    def lin(x0: Double, x1: Double, s: Double): Double =
      x0 * (1.0 - s) + x1 * s
  }
  implicit def doubleToLinearizable(): DoubleIsLinearizable = 
    new DoubleIsLinearizable
    */
}
