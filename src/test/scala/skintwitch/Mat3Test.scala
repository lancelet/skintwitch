package skintwitch

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import Comparisons._

class Mat3Test extends FunSuite with ShouldMatchers {

  private def approxEq(a: Double, b: Double, eps: Double = 1.0e-10): Boolean =
    (a + eps > b) && (a - eps < b)
  
  test("approxEq") {
    val a = Mat3(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val b = Mat3(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val c = Mat3(0, 0, 0, 0, 0, 0, 0, 0, 1)
    Mat3.approxEq(a, b) should be (true)
    Mat3.approxEq(a, c) should be (false)
  }
  
  test("identity") {
    val m = Mat3(1, 0, 0,
                 0, 1 ,0,
                 0, 0, 1)
    Mat3.approxEq(Mat3.identity, m) should be (true)
  }
  
  test("determinant") {
    val b = Mat3(3, 5, 1, 6, 2, 8, 9, 4, 7)
    approxEq(102.0, b.det, 1.0e-10)
  }
  
  test("inverse") {
    val a = Mat3(3, 5, 1, 6, 2, 8, 9, 4, 7)
    val m = Mat3(-0.176471, -0.303922,  0.372549,
                  0.294118,  0.117647, -0.176471,
                  0.058824,  0.323529, -0.235294)
    Mat3.approxEq(m, a.inv, 1e-6) should be (true)
  }
  
  test("transpose") {
    val a = Mat3(1, 2, 3,
                 4, 5, 6,
                 7, 8, 9)
    val m = Mat3(1, 4, 7,
                 2, 5, 8,
                 3, 6, 9)
    Mat3.approxEq(m, a.t) should be (true)
  }
  
  test("multiply by scalar") {
    val a = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val m = Mat3(2, 4, 6, 8, 10, 12, 14, 16, 18)
    Mat3.approxEq(m, a * 2) should be (true)
  }
  
  test("divide by scalar") {
    val a = Mat3(2, 4, 6, 8, 10, 12, 14, 16, 18)
    val m = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    Mat3.approxEq(m, a / 2) should be (true)
  }
  
  test("multiply by matrix") {
    val a = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val b = Mat3(3, 5, 1, 6, 2, 8, 9, 4, 7)
    val m = Mat3(42, 21, 38, 96, 54, 86, 150, 87, 134)
    Mat3.approxEq(m, a * b) should be (true)
  }
  
  test("subtract a matrix") {
    val a = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val b = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val m = Mat3(0, 0, 0, 0, 0, 0, 0, 0, 0)
    Mat3.approxEq(m, a - b) should be (true)
  }
  
  test("add a matrix") {
    val a = Mat3(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val b = Mat3(-1, -2, -3, -4, -5, -6, -7, -8, -9)
    val m = Mat3(0, 0, 0, 0, 0, 0, 0, 0, 0)
    Mat3.approxEq(m, a + b) should be (true)
  }

  test("oneNorm") { pending }
  
  test("infNorm") { pending }
  
  test("polar") {
    // test simple case
    val a = Mat3(1, 2, 3, 4, 5, 6, 6, 5, 5)
    val u = Mat3(-0.11197, -0.42261, 0.89937,
                 -0.16973,  0.89989, 0.40173,
                  0.97911,  0.10767, 0.17249)
    val h = Mat3(5.0838, 3.8229, 3.5412,
                 3.8229, 4.1926, 4.6698,
                 3.5412, 4.6698, 5.9709)

    val (ucalc, hcalc) = a.polar
    Mat3.approxEq(ucalc, u, 1e-5) should be (true)
    Mat3.approxEq(hcalc, h, 1e-4) should be (true)
  }
  
}
