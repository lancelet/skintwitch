package skintwitch

import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scalala.library.LinearAlgebra.det
import scalala.tensor.dense.DenseMatrix
import Comparisons._

class TensorUtilsTest extends FunSuite with ShouldMatchers {

  // principalComp is somewhat tested below in "dgtri", but could do with its
  //  own test
  test("principalComp") (pending)
  
  // polarDecomp is somewhat tested below in "dgtri", but could do with its
  //  own test
  test("polarDecomp") (pending)
  
  test("dgtri") {
    // apply a scaling to a triad of marker coordinates, which are rotated
    //  through some fraction of a full turn in 2D space.  the deformation 
    //  gradient tensor is computed, and then the Biot strain tensor.  the 
    //  Biot tensor's principal components should then be related to the 
    //  original stretches.
    val (sx, sy) = (1.3, 0.9)  // stretch in x and y
    for (i <- 0 until 100) {
      // compute angle, and rotated + scaled vertices of the triangle
      val theta = math.random * math.Pi * 2 * (i / 100.0)
      val (c, s) = (math.cos(theta), math.sin(theta))
      val bx = c * sx
      val by = s * sy
      val cx = s * sx
      val cy = -c * sy
      // f is the deformation gradient tensor
      val f = TensorUtils.dgtri(
        ( 0,  0, 0),
        ( 1,  0, 0),
        ( 0, -1, 0),
        ( 0,  0, 0),
        (bx, by, 0),
        (cx, cy, 0))
      val (r, u) = TensorUtils.polarDecomp(f)
      det(r) should be (1.0 plusOrMinus 1e-6)
      // eBiot is the Biot strain tensor: u - I
      val eBiot = u - DenseMatrix.eye[Double](3)
      // biotPrin are the sorted principal components
      val biotPrin = TensorUtils.principalComp(eBiot).map(_._1).sorted
      require(sx > 0 && sy > 0 && sx > sy)
      eqd(biotPrin, Seq(sy - 1.0, 0.0, sx - 1.0))
    }
  }
  
}
