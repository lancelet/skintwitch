package skintwitch

import scala.collection.immutable._
import scalala.library.Library.normalize
import scalala.library.LinearAlgebra.{ cross, eig, inv, svd }
import scalala.scalar.Complex
import scalala.tensor.{ ::, Matrix }
import scalala.tensor.dense.{ DenseMatrix, DenseVector, DenseVectorCol,
  DenseVectorRow }

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
  def dgtensor(pu: (Double, Double, Double),
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
  
  /** Computes the normal to a cloud of points.
   *  
   *  Given a cloud of points, `q`, and a central point, `p`, this method
   *  finds the average normal vector.  The method uses a small subset of the
   *  computation of the deformation gradient tensor.
   *
   *  @see dgtensor
   *  @param p central point
   *  @param q cloud of points (not including `p`)
   *  @return average normal vector */
  def calcNormal(p: (Double, Double, Double),
                 q: Seq[(Double, Double, Double)]): (Double, Double, Double) =
  {
    implicit def tupleToColVector(x: (Double, Double, Double)) =
      DenseVectorCol(x._1, x._2, x._3)
      
    val x = q.map(_ - p)
    val xbar = x.reduce(_ + _) / x.length.toDouble
    val xbarOP = xbar * xbar.t
    val x00 = (for {
      xitem <- x
    } yield ((xitem * xitem.t) - xbarOP)).reduce(_ + _) / x.length.toDouble
    //val (re, im, vm) = eig(x00)
    //val eigs = re.toArray zip (for (c <- 0 until vm.numCols) yield vm(::, c))
    val eigs = principalComp(x00)
    assert(eigs.length == 3)
    val n = normalize(eigs.minBy(_._1)._2, 2)
    (n(0), n(1), n(2))
  }

  /** Projects a 3D (3x3) tensor to a 2D (2x2) tensor.
   *
   *  The 2D coordinate system, with basis vectors `e1` and `e2` is defined
   *  by:
   *  {{{
   *  e1 = normalize(uvec - uvec dot normal)
   *  e2 = normalize(vvec - vvec dot normal)
   *  e3 = e1 x e2
   *  }}}
   *  The `e3` component is removed by the method.
   *
   *  @param normal normal vector of the 2D plane in the 3D system
   *  @param uvec vector along the x-axis of the 2D system (not necessarily
   *    in the plane defined by `normal`)
   *  @param vvec vector along the y-axis of the 2D system (not necessarily
   *    in the plane defined by `normal`)
   *  @return projected tensor */
  def projectTensorTo2D(normal: (Double, Double, Double),
                        uvec: (Double, Double, Double),
                        vvec: (Double, Double, Double),
                        tensor: Matrix[Double]): Matrix[Double] = 
  {
    implicit def tupleToColVector(x: (Double, Double, Double)) =
      DenseVectorCol(x._1, x._2, x._3)
    def dot3(x: DenseVector[Double], y: DenseVector[Double]) =
      x(0) * y(0) + x(1) * y(1) + x(2) * y(2)

    val e1 = normalize(uvec - dot3(uvec, normal), 2)
    val e2 = normalize(vvec - dot3(vvec, normal), 2)
    val e3 = cross(e1, e2)
    
    val xform = DenseMatrix(e1.toArray, e2.toArray, e3.toArray)
    val t3 = xform * tensor
    
    DenseMatrix(
      Array(t3(0, 0), t3(0, 1)),
      Array(t3(1, 0), t3(1, 1))
    )
  }
  
}
