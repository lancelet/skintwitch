package skintwitch

import math.sqrt
import org.ejml.simple.SimpleMatrix

case class Vec2(x: Double, y: Double) {

  def +(o: Vec2): Vec2 = Vec2(x + o.x, y + o.y)
  def -(o: Vec2): Vec2 = Vec2(x - o.x, y - o.y)
  def *(s: Double): Vec2 = Vec2(x * s, y * s)
  def /(s: Double): Vec2 = Vec2(x / s, y / s)
  
  lazy val length2 = x * x + y * y
  lazy val length = sqrt(length2)
  
}

object Vec2 {
  def fromSimpleMatrix(m: SimpleMatrix): Vec2 = {
    assert(m.getNumElements == 2)
    Vec2(m.get(0), m.get(1))
  }
  val Zero = Vec2(0, 0)
}