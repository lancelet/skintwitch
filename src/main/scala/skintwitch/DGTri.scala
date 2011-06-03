package skintwitch

import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.operators.Implicits._
import scalala.operators._
import scalala.tensor.dense._

object DGTri {

  private implicit def tupleToDenseVector(x: (Double, Double, Double)) =
    DenseVectorCol(x._1, x._2, x._3)
  
  /** Computes deformation gradient tensor on a triangle of markers.
   * 
   *  All parameters as defined in documentation (*u = undeformed),
   *  (*d = deformed). 
   *  
   *  Returns a DGTensor in the original (undeformed) space. */
  def dgtri(au: (Double, Double, Double), 
            bu: (Double, Double, Double),
            cu: (Double, Double, Double),
            ad: (Double, Double, Double),
            bd: (Double, Double, Double),
            cd: (Double, Double, Double)): DGTensor =
  {
    val p = bu - au
    val q = cu - au
    val P = bd - ad
    val Q = cd - ad
    
    val e1 = normalize(p, 2)
    val e3 = normalize(cross(p, q), 2)
    val e2 = -cross(e1, e3)
    val c = DenseMatrix(e1.toArray, e2.toArray, e3.toArray)
    
    val E1 = normalize(P, 2)
    val E3 = normalize(cross(P, Q), 2)
    val E2 = -cross(E1, E3)
    val C = DenseMatrix(E1.toArray, E2.toArray, E3.toArray)
    
    val x1 = c * (DenseMatrix(p.toArray).t)
    val x2 = c * (DenseMatrix(q.toArray).t)
    val X1 = C * (DenseMatrix(P.toArray).t)
    val X2 = C * (DenseMatrix(Q.toArray).t)
    
    val eye2 = DenseMatrix.eye[Double](2)
    val x1x23 = DenseMatrix.horzcat(x1, x2)
    val X1X23 = DenseMatrix.horzcat(X1, X2)
    val x1x2 = x1x23(0 to 1, 0 to 1)
    val X1X2 = X1X23(0 to 1, 0 to 1)
    val F2 = ((eye2 * x1x2.t) \ (eye2 * X1X2.t)).t
    val F = DenseMatrix.eye[Double](3); F(0 to 1, 0 to 1) := F2
    
    val Fu = c.t * F * c
    
    new DGTensor {
      def apply(i: Int, j: Int) = Fu(i, j)
    }
  }
  
}
