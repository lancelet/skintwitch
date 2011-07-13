package skintwitch

import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Comparisons._

class TensorUtilsTest extends FunSuite with ShouldMatchers {

  test("principalComp") (pending)
  
  test("polarDecomp") (pending)
  
  test("calcNormal") {
    // simple example: points in the x-y plane should have z as a normal vector
    val p = Vec3(0.0, 0.0, 0.0)
    val q = Seq(
      Vec3(1.0, 1.0, 0.0),
      Vec3(-1.0, 2.0, 0.0),
      Vec3(5.0, -3.0, 0.0),
      Vec3(8.0, 7.0, 0.0)
    )
    val n = TensorUtils.calcNormal(p, q)
    n.x should be (0.0 plusOrMinus 1e-10)
    n.y should be (0.0 plusOrMinus 1e-10)
    n.z should be (1.0 plusOrMinus 1e-10)
  }
  
  test("projectTensorTo2D") {
    // start with a positive-definite symmetric 3D tensor
    val tensor = Mat3(
       2.0, -1.0,  0.0,
      -1.0,  2.0, -1.0,
       0.0, -1.0,  2.0)
    // now define an orthonormal coordinate basis for the tensor
    val ee1 = Vec3(4.0, 3.0, 8.0)  // kinda random
    val ee2 = Vec3(3.0, -5.0, 3.0) // kinda random
    val e1 = ee1.n
    val e3 = (e1 cross ee2).n
    val e2 = (e3 cross e1).n
    // transform the tensor using its orthonormal basis system into the world
    val xform = Mat3.vertcat(e1, e2, e3).inv
    val txf = xform * tensor * xform.t
    // now project the tensor back to its orthonormal system
    val normal = e3
    val uvec = e1
    val vvec = e2
    val t2 = TensorUtils.projectTensorTo2D(normal, uvec, vvec, txf)
    // check values
    t2.e11 should be ( 2.0 plusOrMinus 1e-8)
    t2.e12 should be (-1.0 plusOrMinus 1e-8)
    t2.e21 should be (-1.0 plusOrMinus 1e-8)
    t2.e22 should be ( 2.0 plusOrMinus 1e-8)
  }
  
}
