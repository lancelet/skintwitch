package skintwitch

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import Comparisons._

class Vec3Test extends FunSuite with ShouldMatchers {

  test("addition") {
    val a = Vec3(1, 2, 3)
    val b = Vec3(-1, -2, -3)
    val c = Vec3(0, 0, 0)
    Vec3.approxEq(c, a + b, 1e-10) should be (true)
  }
  
  test("subtraction") {
    val a = Vec3(1, 2, 3)
    val b = Vec3(1, 2, 3)
    val c = Vec3(0, 0, 0)
    Vec3.approxEq(c, a - b, 1e-10) should be (true)
  }
  
  test("division by a scalar") {
    val a = Vec3(2, 4, 6)
    val c = Vec3(1, 2, 3)
    Vec3.approxEq(c, a / 2.0, 1e-10) should be (true)
  }
  
  test("multiplication by a scalar") { pending }
   
  test("tp (tensor product)") { pending }
  
  test("length2") { pending }
  
  test("length") { pending }
  
  test("n") { pending }
  
  test("dot") { pending }
  
  test("cross") { pending }
  
}