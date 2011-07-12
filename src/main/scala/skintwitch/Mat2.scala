package skintwitch

import org.ejml.simple.SimpleMatrix

case class Mat2(
  e11: Double, e12: Double,
  e21: Double, e22: Double
) {

  def +(o: Mat2): Mat2 = Mat2(e11 + o.e11, e12 + o.e12,
                              e21 + o.e21, e22 + o.e22)
  def /(c: Double): Mat2 = Mat2(e11 / c, e12 / c,
                                e21 / c, e22 / c)
  
  lazy val eig: Seq[(Double, Vec2)] = {
    val evd = simpleMatrix.eig
    val evals = Seq((evd.getEigenvalue(0), 0), (evd.getEigenvalue(1), 1)).
      filter(_._1.isReal)
    evals.map(ev =>
      (ev._1.real, Vec2.fromSimpleMatrix(evd.getEigenVector(ev._2))))
  }
 
  private lazy val simpleMatrix: SimpleMatrix = new SimpleMatrix(Array(
    Array(e11, e12), Array(e21, e22)))
}
