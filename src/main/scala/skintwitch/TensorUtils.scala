package skintwitch

import scala.collection.immutable._

object TensorUtils {
  
  /** Compute the mean of a sequence of matrices.
   * 
   *  @param mats sequence of matrices
   *  @return the mean of the sequence of matrices */
  def mean(mats: Seq[Mat3]): Mat3 = {
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
  def dgtensor(pu: Vec3, qu: Seq[Vec3], pd: Vec3, qd: Seq[Vec3]): Mat3 =
  {
    require(qu.length == qd.length && qu.length >= 2)
        
    // find dx and dX sets: vectors from pu to qu and pd to qd
    val dx = qd.map(_ - pd)
    val dX = qu.map(_ - pu)
    //println("computed dx and dX")
    
    // find averages
    val dxbar = dx.reduce(_ + _) / dx.length.toDouble
    val dXbar = dX.reduce(_ + _) / dX.length.toDouble
    //println("computed dxbar and dXbar")
    
    // marker distribution tensors
    val dXbarOp = dXbar tp dXbar
    val dXbardxbar = dXbar tp dxbar
    val dxbarOp = dxbar tp dxbar
    val x00 = dX.map(dXi => (dXi tp dXi) - dXbarOp).reduce(_ + _) / 
      dX.length.toDouble
    val x01 = (for ((dxi, dXi) <- dx zip dX) yield {
      (dXi tp dxi) - dXbardxbar
    }).reduce(_ + _) / dx.length.toDouble
    val x11 = dx.map(dxi => (dxi tp dxi) - dxbarOp).reduce(_ + _) / 
      dx.length.toDouble
    //println("found marker distribution tensors")
      
    // normals to each set of markers
    val eigs00 = x00.eigSymm
    val e0nN = eigs00.minBy(_._1)
    val eigs11 = x11.eigSymm
    val e1nn = eigs11.minBy(_._1)
    val nN = e0nN._2
    val nn = e1nn._2
    //println("found normals to each set of markers")
    
    // projection tensors
    val pN = Mat3.identity - (nN tp nN)
    val pn = Mat3.identity - (nn tp nn)
    //println("computed projection tensors")
    
    // project x00 and x01
    val x00star = pN * x00 * pN
    val x01star = pn * x01 * pn
    //println("projected x00 and x01")
    
    // compute F
    val x00starinv = (x00star + (nN tp nN)).inv - (nN tp nN)
    val F = pn * x01star.t * x00starinv
    //println("computed F")
    
    // NOTE: when F is computed this way, it has two correct principal
    //       stretches, but the third principal stretch is set to zero, which
    //       is a non-physical state.
    //       Consequently, we re-form F below as a plane-strain tensor, whose
    //       third principal stretch is 1.0

    // decompose F into r and u
    val (r, u) = F.polar
    //println("decomposed F")
    // eigendecompose u, and then re-form with third principal stretch set to
    //  one
    val eigs = u.eigSymm.sortBy(_._1)
    val Q = Mat3.horzcat(eigs(0)._2, eigs(1)._2, eigs(2)._2)
    val L = Mat3.diag(1, eigs(1)._1, eigs(2)._1)
    val Umod = Q * L * Q.inv
    //println("re-formed third principal stretch")
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
  def calcNormal(p: Vec3, q: Seq[Vec3]): Vec3 =
  {
    val x = q.map(_ - p)
    val xbar = x.reduce(_ + _) / x.length.toDouble
    val xbarOP = xbar tp xbar
    val x00 = (for {
      xitem <- x
    } yield ((xitem tp xitem) - xbarOP)).reduce(_ + _) / x.length.toDouble
    val eigs = x00.eigSymm
    eigs.minBy(_._1)._2.n
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
  def projectTensorTo2D(normal: Vec3, uvec: Vec3, vvec: Vec3, tensor: Mat3):
  Mat2 = 
  {
    val e1 = (uvec - normal * (uvec dot normal)).n
    val e2cand = (vvec - normal * (vvec dot normal)).n
    val e3 = e1 cross e2cand
    val e2 = e3 cross e1

    val xform = Mat3.vertcat(e1, e2, e3)
    val t3 = xform * tensor * (xform.t)

    Mat2(t3.e11, t3.e12,
         t3.e21, t3.e22)
  }
  
}
