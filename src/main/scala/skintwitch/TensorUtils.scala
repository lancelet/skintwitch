package skintwitch

import scala.collection.immutable._
import scalala.library.LinearAlgebra.eig
import scalala.scalar.Complex
import scalala.tensor.{ ::, Matrix }
import scalala.tensor.dense.DenseVector

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
  
}