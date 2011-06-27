package skintwitch

/** Linearizable type, which can be interpolated and extrapolated using a
 *  single parameter. */
trait Linearizable[T] {
  /** Interpolates / extrapolates between `x0` and `x1` using the equation 
   *  `x0 * (1-s) + x1 * s`. */
  def lin(x0: T, x1: T, s: Double): T
}

object Linearizable {
  class DoubleIsLinearizable(d: Double) extends Linearizable[Double] {
    def lin(x0: Double, x1: Double, s: Double): Double =
      x0 * (1.0 - s) + x1 * s
  }
  
  implicit def doubleToLinearizable(d: Double) = new DoubleIsLinearizable(d)
}
