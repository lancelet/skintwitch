package skintwitch

import org.ejml.simple.SimpleMatrix

case class Mat2(
  e11: Double, e12: Double,
  e21: Double, e22: Double
) {
  import Mat2._
  
  def +(o: Mat2): Mat2 = Mat2(e11 + o.e11, e12 + o.e12,
                              e21 + o.e21, e22 + o.e22)
  def -(o: Mat2): Mat2 = Mat2(e11 - o.e11, e12 - o.e12,
                              e21 - o.e21, e22 - o.e22)
  def /(c: Double): Mat2 = Mat2(e11 / c, e12 / c,
                                e21 / c, e22 / c)
  def *(c: Double): Mat2 = Mat2(e11 * c, e12 * c,
                                e21 * c, e22 * c)
  
  def *(o: Mat2): Mat2 = Mat2(e11*o.e11 + e12*o.e21, e11*o.e12 + e12*o.e22,
                              e21*o.e11 + e22*o.e21, e21*o.e12 + e22*o.e22)
                                
  lazy val eig: Seq[(Double, Vec2)] = {
    val evd = simpleMatrix.eig
    val evals = Seq((evd.getEigenvalue(0), 0), (evd.getEigenvalue(1), 1)).
      filter(_._1.isReal)
    evals.map(ev =>
      (ev._1.real, Vec2.fromSimpleMatrix(evd.getEigenVector(ev._2))))
  }
 
  lazy val trace: Double = e11 + e22
  
  private lazy val simpleMatrix: SimpleMatrix = new SimpleMatrix(Array(
    Array(e11, e12), Array(e21, e22)))
  
  lazy val det: Double = e11 * e22 - e12 * e21
  
  lazy val inv: Mat2 = Mat2(e22, -e12, -e21, e11) / det
  
  lazy val t: Mat2 = Mat2(e11, e21, e12, e22)
  
  /** Polar decomposition: this = R * U, where R is a rotation matrix . */
  lazy val polar: (Mat2, Mat2) = {
    val svd = simpleMatrix.svd
    val (u, w, v) = (fromSimpleMatrix(svd.getU),
                     fromSimpleMatrix(svd.getW),
                     fromSimpleMatrix(svd.getV))
    (u * v.t, v * w * v.t)
  }
  
}

object Mat2 {
  val Identity: Mat2 = Mat2(1, 0, 0, 1)
  def fromSimpleMatrix(sm: SimpleMatrix): Mat2 = {
    assert(sm.numCols == 2 && sm.numRows == 2)
    Mat2(sm.get(0, 0), sm.get(0, 1),
         sm.get(1, 0), sm.get(1, 1))
  }  
}