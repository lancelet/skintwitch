package skintwitch

import org.ejml.simple.SimpleMatrix

case class Vec2(x: Double, y: Double) {

}

object Vec2 {
  def fromSimpleMatrix(m: SimpleMatrix): Vec2 = {
    assert(m.getNumElements == 2)
    Vec2(m.get(0), m.get(1))
  }
}