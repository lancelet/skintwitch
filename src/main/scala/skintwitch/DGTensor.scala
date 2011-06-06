package skintwitch

import scala.collection.immutable._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.scalar.{ Complex, Scalar }
import scalala.tensor.::
import scalala.tensor.Matrix
import scalala.tensor.domain.{IndexDomain, Domain2}
import scalala.tensor.dense._

/** Deformation gradient tensor. 
 *  
 *  This trait knows it's a 3x3 matrix.  Just need to give it an apply(i,j)
 *  function. */
trait DGTensor extends Matrix[Double] {
  // All stuff required by Scalala
  val numCols = 3
  val numRows = 3
  val scalar = Scalar.scalarD
  
  /** Principal stretches and directions. */
  lazy val prin: Seq[(Double, DenseVector[Double])] = {
    val (r, c, m) = eig(this)
    // join real and imaginary parts to a sequence of complex
    val cpx = (r.data zip c.data).map(rc => Complex(rc._1, rc._2))
    // separate out the directions
    val d = for (c <- 0 until m.numCols) yield normalize(m(::, c), 2)
    // remove any eigenvalues with large imaginary parts
    val cTol = 1e-6
    (cpx zip d).filter(_._1.imag < cTol).map(cd => (cd._1.real, cd._2)).toList
  }
}

object DGTensor {
  def mean(ts: Seq[DGTensor]): DGTensor = {
    val backingMatrix = (ts: Seq[Matrix[Double]]).fold(
        DenseMatrix.zeros[Double](3, 3)
      )(_ + _) / ts.length
    new DGTensor { def apply(i: Int, j: Int) = backingMatrix(i, j) }
  }
}
