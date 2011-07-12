package skintwitch

import scala.collection.immutable._
import org.ejml.simple.SimpleMatrix
import org.ejml.alg.dense.decomposition.DecompositionFactory

case class Mat3(
  e11: Double, e12: Double, e13: Double,
  e21: Double, e22: Double, e23: Double,
  e31: Double, e32: Double, e33: Double
) {
  import Mat3._
  
  lazy val det: Double = e11 * (e22 * e33 - e23 * e32) -
                         e12 * (e21 * e33 - e23 * e31) +
                         e13 * (e21 * e32 - e22 * e31)
     
  lazy val inv: Mat3 = Mat3(e33 * e22 - e32 * e23,
                            -(e33 * e12 - e32 * e13),
                            e23 * e12 - e22 * e13,
                            -(e33 * e21 - e31 * e23),
                            e33 * e11 - e31 * e13,
                            -(e23 * e11 - e21 * e13),
                            e32 * e21 - e31 * e22,
                            -(e32 * e11 - e31 * e12),
                            e22 * e11 - e21 * e12) / det

  lazy val t: Mat3 = Mat3(e11, e21, e31,
                          e12, e22, e32,
                          e13, e23, e33)
  
  lazy val oneNorm: Double = max3(abs(e11) + abs(e21) + abs(e31),
                                  abs(e12) + abs(e22) + abs(e32),
                                  abs(e13) + abs(e23) + abs(e33))

  lazy val infNorm: Double = max3(abs(e11) + abs(e12) + abs(e13),
                                  abs(e21) + abs(e22) + abs(e23),
                                  abs(e31) + abs(e32) + abs(e33))

  def -(m: Mat3): Mat3 = Mat3(
      e11 - m.e11, e12 - m.e12, e13 - m.e13,
      e21 - m.e21, e22 - m.e22, e23 - m.e23,
      e31 - m.e31, e32 - m.e32, e33 - m.e33)
      
  def +(m: Mat3): Mat3 = Mat3(
      e11 + m.e11, e12 + m.e12, e13 + m.e13,
      e21 + m.e21, e22 + m.e22, e23 + m.e23,
      e31 + m.e31, e32 + m.e32, e33 + m.e33)

  def *(c: Double): Mat3 = Mat3(e11 * c, e12 * c, e13 * c,
                                e21 * c, e22 * c, e23 * c,
                                e31 * c, e32 * c, e33 * c)
  def /(c: Double): Mat3 = Mat3(e11 / c, e12 / c, e13 / c,
                                e21 / c, e22 / c, e23 / c,
                                e31 / c, e32 / c, e33 / c)
                                 
  def *(m: Mat3): Mat3 = Mat3(
        // row 1
        e11 * m.e11 + e12 * m.e21 + e13 * m.e31,
        e11 * m.e12 + e12 * m.e22 + e13 * m.e32,
        e11 * m.e13 + e12 * m.e23 + e13 * m.e33,
        // row 2
        e21 * m.e11 + e22 * m.e21 + e23 * m.e31,
        e21 * m.e12 + e22 * m.e22 + e23 * m.e32,
        e21 * m.e13 + e22 * m.e23 + e23 * m.e33,
        // row 3
        e31 * m.e11 + e32 * m.e21 + e33 * m.e31,
        e31 * m.e12 + e32 * m.e22 + e33 * m.e32,
        e31 * m.e13 + e32 * m.e23 + e33 * m.e33)
  
  /** Polar decomposition. */
  lazy val polar: (Mat3, Mat3) = {
    val svd = simpleMatrix.svd
    val (u, w, v) = (fromSimpleMatrix(svd.getU),
                     fromSimpleMatrix(svd.getW),
                     fromSimpleMatrix(svd.getV))
    (u * v.t, v * w * v.t)
  }
  
  /** Eigenvalues. */
  lazy val eig: Seq[(Double, Vec3)] = {
    val evd = simpleMatrix.eig
    val evals = Seq((evd.getEigenvalue(0), 0), 
                    (evd.getEigenvalue(1), 1), 
                    (evd.getEigenvalue(2), 2)).filter(_._1.isReal)
    evals.map(ev =>
      (ev._1.real, Vec3.fromSimpleMatrix(evd.getEigenVector(ev._2))))
  }
  
  /** Eigenvalues when symmetric (faster). */
  lazy val eigSymm: Seq[(Double, Vec3)] = {
    val evd = DecompositionFactory.eigSymm(3, true)
    val retVal = evd.decompose(simpleMatrix.getMatrix)
    assert(retVal == true)
    val evals = Seq((evd.getEigenvalue(0), 0), 
                    (evd.getEigenvalue(1), 1), 
                    (evd.getEigenvalue(2), 2)).filter(_._1.isReal)
    evals.map(ev =>
      (ev._1.real, Vec3.fromSimpleMatrix(
          SimpleMatrix.wrap(evd.getEigenVector(ev._2)))))   
  }
  
  private lazy val simpleMatrix: SimpleMatrix = new SimpleMatrix(Array(
    Array(e11, e12, e13), Array(e21, e22, e23), Array(e31, e32, e33)))
  
}

object Mat3 {
  def fromSimpleMatrix(sm: SimpleMatrix): Mat3 = {
    assert(sm.numCols == 3 && sm.numRows == 3)
    Mat3(sm.get(0, 0), sm.get(0, 1), sm.get(0, 2),
         sm.get(1, 0), sm.get(1, 1), sm.get(1, 2),
         sm.get(2, 0), sm.get(2, 1), sm.get(2, 2))
  }
  val identity: Mat3 = Mat3(1,0,0, 0,1,0, 0,0,1)
  def approxEq(a: Mat3, b: Mat3, eps: Double = 1.0e-8): Boolean = {
    def approxEq(a: Double, b: Double, eps: Double) =
      (a + eps > b) && (a - eps < b)
    approxEq(a.e11, b.e11, eps) &&
    approxEq(a.e12, b.e12, eps) &&
    approxEq(a.e13, b.e13, eps) &&
    approxEq(a.e21, b.e21, eps) &&
    approxEq(a.e22, b.e22, eps) &&
    approxEq(a.e23, b.e23, eps) &&
    approxEq(a.e31, b.e31, eps) &&
    approxEq(a.e32, b.e32, eps) &&
    approxEq(a.e33, b.e33, eps)
  }
  def horzcat(a: Vec3, b: Vec3, c: Vec3): Mat3 = Mat3(
    a.x, b.x, c.x, a.y, b.y, c.y, a.z, b.z, c.z)
  def vertcat(a: Vec3, b: Vec3, c: Vec3): Mat3 = Mat3(
    a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z)
  def diag(a: Double, b: Double, c: Double): Mat3 = Mat3(
    a, 0, 0, 0, b, 0, 0, 0, c)
  private def abs(x: Double): Double = if (x >= 0) x else -x
  private def max3(a: Double, b: Double, c: Double): Double = {
    if (a > b && a > c) {
      a
    } else if (b > c) {
      b
    } else {
      c
    }
  }
}
