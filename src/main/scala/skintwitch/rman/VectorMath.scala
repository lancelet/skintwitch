package skintwitch.rman

import math.{cos, Pi, sin, sqrt}
import skintwitch.Linearizable
import simplex3d.math.double.Mat4
import breeze.linalg.inv
import breeze.linalg.DenseMatrix

/** 3D vector. */
case class V3(e0: Double, e1: Double, e2: Double) {
  def x(o: V3) = V3(e1 * o.e2 - e2 * o.e1, 
                    e2 * o.e0 - e0 * o.e2,
                    e0 * o.e1 - e1 * o.e0)
  def length2 = e0 * e0 + e1 * e1 + e2 * e2
  def length = sqrt(e0 * e0 + e1 * e1 + e2 * e2)
  def normalized = V3(e0 / length, e1 / length, e2 / length)
  def -(o: V3) = V3(e0 - o.e0, e1 - o.e1, e2 - o.e2)
  def +(o: V3) = V3(e0 + o.e0, e1 + o.e1, e2 + o.e2)
  def *(c: Double) = V3(e0 * c, e1 * c, e2 * c)
  def dot(o: V3) = e0 * o.e0 + e1 * o.e1 + e2 * o.e2
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
  def *(m: M4) = R4(
    e0 * m.e00 + e1 * m.e10 + e2 * m.e20 + e3 * m.e30,
    e0 * m.e01 + e1 * m.e11 + e2 * m.e21 + e3 * m.e31,
    e0 * m.e02 + e1 * m.e12 + e2 * m.e22 + e3 * m.e32,
    e0 * m.e03 + e1 * m.e13 + e2 * m.e23 + e3 * m.e33)
}
object R4 {
  def apply(v3: V3, w: Double): R4 = R4(v3.e0, v3.e1, v3.e2, w)
  implicit def R4toV3(r4: R4): V3 = 
    V3(r4.e0 / r4.e3, r4.e1 / r4.e3, r4.e2 / r4.e3)
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
  lazy val inv = {
    val m = DenseMatrix(
      Array(e00, e01, e02, e03),
      Array(e10, e11, e12, e13),
      Array(e20, e21, e22, e23),
      Array(e30, e31, e32, e33))
    val q = breeze.linalg.inv(m)
    M4(q(0,0), q(0,1), q(0,2), q(0,3),
       q(1,0), q(1,1), q(1,2), q(1,3),
       q(2,0), q(2,1), q(2,2), q(2,3),
       q(3,0), q(3,1), q(3,2), q(3,3))
  }
}
object M4 {
  val identity = M4(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1)
  def scale(sx: Double, sy: Double, sz: Double) = M4(
    sx,  0,  0, 0,
     0, sy,  0, 0,
     0,  0, sz, 0,
     0,  0,  0, 1)
  def translate(tx: Double, ty: Double, tz: Double) = M4(
     1,  0,  0, 0,
     0,  1,  0, 0,
     0,  0,  1, 0,
    tx, ty, tz, 1)
  /** Rotate by a given angle (in degrees) about a given axis. */
  def rotate(angle: Double, dx: Double, dy: Double, dz: Double) = {
    val angRad = angle * Pi / 180.0  /* may need to be negative */
    val c = cos(angRad)
    val s = sin(angRad)
    val C = 1.0 - c
    val len = sqrt(dx*dx + dy*dy + dz*dz)
    val x = dx / len
    val y = dy / len
    val z = dz / len
    M4(  x*x*C+c, y*x*C+z*s, z*x*C-y*s, 0,
       x*y*C-z*s,   y*y*C+c, z*y*C+x*s, 0,
       x*z*C+y*s, y*z*C-x*s,   z*z*C+c, 0,
               0,         0,         0, 1)
  }
  implicit def toMat4(m: M4): Mat4 = Mat4(
    m.e00, m.e01, m.e02, m.e03,
    m.e10, m.e11, m.e12, m.e13,
    m.e20, m.e21, m.e22, m.e23,
    m.e30, m.e31, m.e32, m.e33)
}
