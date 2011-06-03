package skintwitch

import scala.collection.immutable._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.scalar.Scalar
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
    //val cTol = 1e-6
    //assert(c.data.forall(_ < cTol))  // no complex values allowed
    val s = r.data  // principal stretches
    val d = for (c <- 0 until m.numCols) yield normalize(m(::, c), 2)  // dir
    (s zip d).toList
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
