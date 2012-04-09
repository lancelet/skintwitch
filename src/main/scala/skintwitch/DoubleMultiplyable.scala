package skintwitch

/** Type that can be multiplied by a Double. */
trait DoubleMultiplyable[T] extends Fractional[T] {
  def multiplyDouble(x: T, y: Double): T
}

object DoubleMultiplyable {
  class DoubleIsDoubleMultiplyable extends Numeric.DoubleIsFractional 
  with DoubleMultiplyable[Double] with Ordering.DoubleOrdering {
    def multiplyDouble(x: Double, y: Double): Double = x * y
  }  
  implicit object DoubleIsDoubleMultiplyable extends DoubleIsDoubleMultiplyable
}