package skintwitch.rman

import math.sqrt
import skintwitch.Linearizable

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
object V3 {
  def apply(a: Array[Double]): V3 = {
    require(a.length == 3)
    V3(a(0), a(1), a(2))
  }
  class V3isLinearizable() extends Linearizable[V3] {
    def lin(x0: V3, x1: V3, s: Double): V3 = x0 * (1.0 - s) + x1 * s
  }
  implicit def v3ToLinearizable(): Linearizable[V3] = new V3isLinearizable()
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
