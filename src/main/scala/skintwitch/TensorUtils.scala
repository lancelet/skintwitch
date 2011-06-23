package skintwitch

import scala.collection.immutable._
import scalala.library.Library.normalize
import scalala.library.LinearAlgebra.{ cross, eig, inv, svd }
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

  /** Polar decomposition: `m = r * u`.
   * 
   *  Performs a polar decomposition of tensor `m` into an orthonormal matrix
   *  `r` and a positive definite symmetric tensor, `u`.
   *  
   *  @param m tensor matrix on which to perform the polar decomposition
   *  @return `(r, u)` */
  def polarDecomp(m: Matrix[Double]): (Matrix[Double], Matrix[Double]) = {
    require(m.numRows == 3 && m.numCols == 3)
    val (w, sdiag, vp) = svd(DenseMatrix.horzcat(m))
    val r = w * vp
    val s = DenseMatrix.tabulate(3, 3)((r: Int, c: Int) => 
      if (r != c) 0.0 else sdiag(r))
    val u = vp.t * s * vp
    (r, u)
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
  
  /** Compute the mean of a sequence of matrices.
   * 
   *  @param mats sequence of matrices
   *  @return the mean of the sequence of matrices */
  def mean(mats: Seq[Matrix[Double]]): Matrix[Double] = {
    mats.reduce(_ + _) / mats.length.toDouble
  }
  
  /** Finds deformation gradient tensor using the Peters method.
   * 
   *  TODO: Optimise the method; several multiplications occur multiple times,
   *        etc.
   * 
   *  The method is described in the following publications:
   *  
   *  Peters, G. (1987) Tools for the measurement of stress and strain fields
   *    in soft tissue; application to the elbow joint.  PhD Thesis, Eindhoven
   *    University of Technology.
   *    
   *  Kim, W. and Kohles, S.S. (2009) Optical acquisition and polar
   *    decomposition of the full-field deformation gradient tensor within
   *    a fracture callus.  J Biomech. 42:2026-2032.
   *  
   *  Geers, M.G.D., de Borst, R. and Brekelmans, W.A.M. (1996) Computing
   *    strain fields from discrete displacement fields in 2D-solids. Int.
   *    J. Solids Structures. 33(29):4293-4307.
   * 
   *  @param pu: un-deformed central marker
   *  @param qu: un-deformed F-group
   *  @param pd: deformed central marker
   *  @param qd: deformed F-group
   *  @return deformation gradient tensor relative to the undeformed
   *    markers (I think it's relative to the undeformed markers...) */
  def peters(pu: (Double, Double, Double),
             qu: Seq[(Double, Double, Double)],
             pd: (Double, Double, Double),
             qd: Seq[(Double, Double, Double)]): Matrix[Double] =
  {
    require(qu.length == qd.length && qu.length >= 2)
   
    // TODO: Figure out why this is required.  It handles the subtractions
    //       immediately below (bu - au, etc.).  However,
    //       scalala.operators.Implicits._ should also handle this, but it
    //       doesn't!
    implicit def tupleToVector(x: (Double, Double, Double)) =
      DenseVector(x._1, x._2, x._3)
        
    // find dx and dX sets: vectors from pu to qu and pd to qd
    val dx = qd.map(_ - pd)
    val dX = qu.map(_ - pu)
    
    // find averages
    val dxbar = dx.reduce(_ + _) / dx.length.toDouble
    val dXbar = dX.reduce(_ + _) / dX.length.toDouble
    
    // marker distribution tensors
    val dXbarOp = dXbar * dXbar.t
    val dXbardxbar = dXbar * dxbar.t
    val dxbarOp = dxbar * dxbar.t
    val x00 = dX.map(dXi => dXi * dXi.t - dXbarOp).reduce(_ + _) / 
      dX.length.toDouble
    val x01 = (for ((dxi, dXi) <- dx zip dX) yield {
      dXi * dxi.t - dXbardxbar
    }).reduce(_ + _) / dx.length.toDouble
    val x11 = dx.map(dxi => dxi * dxi.t - dxbarOp).reduce(_ + _) / 
      dx.length.toDouble
      
    // normals to each set of markers
    val eigs00 = principalComp(x00)
    val e0nN = eigs00.minBy(_._1)
    val eigs11 = principalComp(x11)
    val e1nn = eigs11.minBy(_._1)
    val nNmat = DenseMatrix(e0nN._2.toArray).t
    val nnmat = DenseMatrix(e1nn._2.toArray).t
    
    // projection tensors
    val eye3 = DenseMatrix.eye[Double](3)
    val pN = eye3 - nNmat * nNmat.t
    val pn = eye3 - nnmat * nnmat.t
    
    // project x00 and x01
    val x00star = pN * x00 * pN
    val x01star = pn * x01 * pn
    
    // compute F
    val x00starinv = inv(x00star + nNmat * nNmat.t) - nNmat * nNmat.t
    val F = pn * x01star.t * x00starinv
    
    // NOTE: when F is computed this way, it has two correct principal
    //       stretches, but the third principal stretch is set to zero, which
    //       is a non-physical state.
    //       Consequently, we re-form F below as a plane-strain tensor, whose
    //       third principal stretch is 1.0
    
    // decompose F into r and u
    val (r, u) = polarDecomp(F)
    // eigendecompose u, and then re-form with third principal stretch set to
    //  one
    val eigs = principalComp(u).sortBy(_._1)
    val Q = DenseMatrix.horzcat(DenseMatrix(eigs(0)._2.toArray).t, 
                                DenseMatrix(eigs(1)._2.toArray).t, 
                                DenseMatrix(eigs(2)._2.toArray).t)
    val L = DenseMatrix(Array(1.0, 0, 0), /* new (1.0) principal stretch */
                        Array(0, eigs(1)._1, 0), /* 2nd prin stretch */
                        Array(0, 0, eigs(2)._1)  /* 3rd prin stretch */)
    val Umod = Q * L * inv(Q)
    r * Umod
  }
  
}
