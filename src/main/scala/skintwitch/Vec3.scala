package skintwitch

import math.sqrt
import org.ejml.simple.SimpleMatrix
import mocaputils.{Vec3 => MVec3}

trait Vec3 {
  def x: Double
  def y: Double
  def z: Double

  def +(o: Vec3): Vec3
  def -(o: Vec3): Vec3
  def /(c: Double): Vec3
  def *(c: Double): Vec3
  def tp(o: Vec3): Mat3
  def dot(o: Vec3): Double
  def cross(o: Vec3): Vec3
  def length2: Double
  def length: Double
  def n: Vec3  
}

trait Vec3Ops {
  def x: Double
  def y: Double
  def z: Double
  
  def +(o: Vec3) = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3) = Vec3(x - o.x, y - o.y, z - o.z)
  def /(c: Double) = Vec3(x / c, y / c, z / c)
  def *(c: Double) = Vec3(x * c, y * c, z * c)
  def tp(o: Vec3) = Mat3(x * o.x, x * o.y, x * o.z,
                         y * o.x, y * o.y, y * o.z,
                         z * o.x, z * o.y, z * o.z)
  def dot(o: Vec3) = x * o.x + y * o.y + z * o.z
  def cross(o: Vec3) = Vec3(y * o.z - z * o.y,
                            z * o.x - x * o.z,
                            x * o.y - y * o.x)
  lazy val length2 = x * x + y * y + z * z
  lazy val length = sqrt(length2)
  lazy val n: Vec3 = Vec3(x / length, y / length, z / length)  
}

object Vec3 {
  
  private final case class SimpleVec3(x: Double, y: Double, z: Double) 
  extends Vec3 with Vec3Ops
  
  private final case class MocapUtilsVec3(v: MVec3) extends Vec3 with Vec3Ops {
    def x: Double = v.x
    def y: Double = v.y
    def z: Double = v.z
  }
  
  implicit def tupleToVec3(t: (Double, Double, Double)): Vec3 =
    SimpleVec3(t._1, t._2, t._3)
  def apply(t: (Double, Double, Double)): Vec3 = SimpleVec3(t._1, t._2, t._3)
  def apply(v: MVec3): Vec3 = MocapUtilsVec3(v)
  def approxEq(a: Vec3, b: Vec3, eps: Double = 1.0e-8): Boolean = {
    def approxEq(a: Double, b: Double, eps: Double) =
      (a + eps > b) && (a - eps < b)
    approxEq(a.x, b.x, eps) &&
    approxEq(a.y, b.y, eps) &&
    approxEq(a.z, b.z, eps)
  }
  def fromSimpleMatrix(m: SimpleMatrix): Vec3 = {
    assert(m.getNumElements == 3)
    Vec3(m.get(0), m.get(1), m.get(2))
  }
  val Zero = Vec3(0, 0, 0)
}