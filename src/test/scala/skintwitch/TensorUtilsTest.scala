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
  
}
