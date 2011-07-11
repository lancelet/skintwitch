package skintwitch

import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scalala.library.Library.normalize
import scalala.library.LinearAlgebra.{ cross, det, inv }
import scalala.tensor.dense.{ DenseMatrix, DenseVectorCol }
import Comparisons._

class TensorUtilsTest extends FunSuite with ShouldMatchers {

  test("principalComp") (pending)
  
  test("polarDecomp") (pending)
  
  test("calcNormal") {
    // simple example: points in the x-y plane should have z as a normal vector
    val p = (0.0, 0.0, 0.0)
    val q = Seq(
      (1.0, 1.0, 0.0),
      (-1.0, 2.0, 0.0),
      (5.0, -3.0, 0.0),
      (8.0, 7.0, 0.0)
    )
    val n = TensorUtils.calcNormal(p, q)
    n._1 should be (0.0 plusOrMinus 1e-10)
    n._2 should be (0.0 plusOrMinus 1e-10)
    n._3 should be (1.0 plusOrMinus 1e-10)
  }
  
  test("projectTensorTo2D") {
    // start with a positive-definite symmetric 3D tensor
    val tensor = DenseMatrix(
      Array( 2.0, -1.0,  0.0),
      Array(-1.0,  2.0, -1.0),
      Array( 0.0, -1.0,  2.0)
    )
    // now define an orthonormal coordinate basis for the tensor
    val ee1 = DenseVectorCol(4.0, 3.0, 8.0)  // kinda random
    val ee2 = DenseVectorCol(3.0, -5.0, 3.0) // kinda random
    val e1 = normalize(ee1, 2)
    val e3 = normalize(cross(e1, ee2), 2)
    val e2 = normalize(cross(e3, e1), 2)
    // transform the tensor using its orthonormal basis system into the world
    val xform = inv(DenseMatrix(e1.toArray, e2.toArray, e3.toArray))
    val txf = xform * tensor
    // now project the tensor back to its orthonormal system
    val normal = (e3(0), e3(1), e3(2))
    val uvec = (e1(0), e1(1), e1(2))
    val vvec = (e2(0), e2(1), e2(2))
    val t2 = TensorUtils.projectTensorTo2D(normal, uvec, vvec, txf)
    // check values
    t2(0, 0) should be ( 2.0 plusOrMinus 1e-8)
    t2(0, 1) should be (-1.0 plusOrMinus 1e-8)
    t2(1, 0) should be (-1.0 plusOrMinus 1e-8)
    t2(1, 1) should be ( 2.0 plusOrMinus 1e-8)
  }
  
}
