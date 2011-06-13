package skintwitch

import scala.collection.immutable._
import scalala.library.Library.normalize
import scalala.library.LinearAlgebra.{ cross, eig }
import scalala.scalar.Complex
import scalala.tensor.{ ::, Matrix }
import scalala.tensor.dense.{ DenseMatrix, DenseVector }

object TensorUtils {
  
  /** Finds principal values and principal directions of a second-order tensor.
   *  
   *  When expressed as a matrix, the principal values of the tensor are the 
   *  real eigenvalues, and the principal directions are the corresponding
   *  eigenvectors.
   *  
   *  @param m matrix representation of the second-order tensor
   *  @param cThreshold threshold above which imaginary components of 
   *    eigenvalues are assumed to make them non-real
   *    
   *  @return sequence of principal values and corresponding principal 
   *    directions */
  def principalComp(m: Matrix[Double], cThreshold: Double = 1e-6): 
  Seq[(Double, DenseVector[Double])] = {
    // eigenvalue decomposition of m
    val (re, im, vm) = eig(m)
    // decompose matrix of eigenvectors to a sequence, and marshal complex vals
    val vs = for (c <- 0 until vm.numCols) yield vm(::, c)
    val ev = for ((r, i) <- re.data zip im.data) yield Complex(r, i)
    // discard eigenvalues with imaginary parts >= cThreshold
    (ev zip vs).filter(x => x._1.imag < cThreshold).
      map(x => (x._1.real, x._2)).toList
  }
  

  /** Computes surface deformation gradient tensor from triads of markers in
   *  un-deformed and deformed configurations of a surface.
   *  
   *  This method takes a triad of marker coordinates in an un-deformed state
   *  (`au`, `bu` and `cu`), and a triad of corresponding marker coordinates 
   *  in a deformed state (`ad`, `bd` and `cd`).  The markers are assumed to
   *  lie on the surface of a body which has deformed.  The deformation 
   *  gradient tensor over the region represented by the markers is found by 
   *  computing a least-squares solution to the matrix equation:
   *  {{{
   *  dX = F dx 
   *  }}}
   *  Where `F` is the deformation gradient tensor, `dx` is an infinitesimal
   *  line segment in the un-deformed body, and `dX` is an infinitesimal line
   *  segment in the deformed body.  `dx` and `dX` are approximated by the
   *  vectors `a->b` and `a->c` in the undeformed and deformed states.
   * 
   *  The marker coordinates must all be expressed in the same coordinate
   *  system.  However, because the markers may have experienced a net rigid
   *  body rotation between the un-deformed and deformed states, the method
   *  must be told whether to return a tensor which is expressed relative to
   *  the un-deformed system or the deformed system.  By default, when
   *  `undeformedCoordSys = true`, the tensor is expressed relative to the
   *  un-deformed markers.
   * 
   *  @param au point in the un-deformed system
   *  @param bu point in the un-deformed system
   *  @param cu point in the un-deformed system
   *  @param ad point in the deformed system
   *  @param bd point in the deformed system
   *  @param cd point in the deformed system
   *  @param undeformedCoordSys if `true`, return the tensor in the
   *    coordinate system relative to the un-deformed markers; if `false`,
   *    return the tensor in the coordinate system relative to the deformed
   *    markers 
   *  @return deformation gradient tensor (as a 3x3 matrix) in either the
   *    un-deformed (`undeformedCoordSys = true`) or
   *    deformed (`undeformedCoordSys = false`) coordinate system */
  def dgtri(au: (Double, Double, Double),
            bu: (Double, Double, Double),
            cu: (Double, Double, Double),
            ad: (Double, Double, Double),
            bd: (Double, Double, Double),
            cd: (Double, Double, Double),
            undeformedCoordSys: Boolean = true): Matrix[Double] =
  {
    // TODO: Figure out why this is required.  It handles the subtractions
    //       immediately below (bu - au, etc.).  However,
    //       scalala.operators.Implicits._ should also handle this, but it
    //       doesn't!
    implicit def tupleToVector(x: (Double, Double, Double)) =
      DenseVector(x._1, x._2, x._3)
    
    // vectors p,q and P,Q approximate infinitesimal line segments before and
    //   after deformation
    val p = bu - au
    val q = cu - au
    val P = bd - ad
    val Q = cd - ad
    
    // e1,e2,e3 are an orthonormal coordinate system basis for the un-deformed
    //  markers, and c is a rotation matrix from the un-deformed system to the
    //  e1,e2,e3 system.
    val e1 = normalize(p, 2)
    val e3 = normalize(cross(p, q), 2)
    val e2 = -cross(e1, e3)
    val c = DenseMatrix(e1.toArray, e2.toArray, e3.toArray)
    
    // E1,E2,E3 are an orthonormal coordinate system basis for the deformed
    //  markers, and C is a rotation matrix from the deformed system to the
    //  E1,E2,E3 system.
    val E1 = normalize(P, 2)
    val E3 = normalize(cross(P, Q), 2)
    val E2 = -cross(E1, E3)
    val C = DenseMatrix(E1.toArray, E2.toArray, E3.toArray)
    
    // transform p,q and P,Q to the same planar coordinate system, removing 
    //  any relative rotation between p and P
    val x1 = c * (DenseMatrix(p.toArray).t)
    val x2 = c * (DenseMatrix(q.toArray).t)
    val X1 = C * (DenseMatrix(P.toArray).t)
    val X2 = C * (DenseMatrix(Q.toArray).t)
    
    // compute F, the deformation gradient tensor
    val eye2 = DenseMatrix.eye[Double](2)
    val x1x23 = DenseMatrix.horzcat(x1, x2)
    val X1X23 = DenseMatrix.horzcat(X1, X2)
    val x1x2 = x1x23(0 to 1, 0 to 1)
    val X1X2 = X1X23(0 to 1, 0 to 1)
    val F2 = ((eye2 * x1x2.t) \ (eye2 * X1X2.t)).t
    val F = DenseMatrix.eye[Double](3); F(0 to 1, 0 to 1) := F2
    
    // transform F back to one of the original systems of coordinates
    if (undeformedCoordSys) {
      c.t * F * c
    } else {
      C.t * F * C
    }
  }  
  
}