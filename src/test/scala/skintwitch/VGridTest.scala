package skintwitch

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class VGridTest extends FunSuite with ShouldMatchers {

  test("construction from an existing Grid") {
    val testGrid = new Grid[Int] {
      private val a = Array(Array(1,2,3), Array(4,5,6))
      val numRows = 2
      val numCols = 3
      def apply(row: Int, col: Int) = a(row)(col)
    }
    val g = VGrid(testGrid)
    g.numRows should be (2)
    g.numCols should be (3)
    g(0, 0) should be (1)
    g(0, 1) should be (2)
    g(0, 2) should be (3)
    g(1, 0) should be (4)
    g(1, 1) should be (5)
    g(1, 2) should be (6)
  }
  
  test("failure should occur for zero-size grid") {
    intercept [IllegalArgumentException] {
      new VGrid(Vector(Vector.empty[Int]))
    }
  }
  
  test("failure should occur if construction sub-vectors are diff lengths") {
    val v = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8))
    intercept [IllegalArgumentException] {
      new VGrid(v)
    }
  }
  
  test("map") {
    val vg = VGrid(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))
    val r = vg.map(_ - 1)
    r.numRows should be (2)
    r.numCols should be (3)
    r(0, 0) should be (0)
    r(0, 1) should be (1)
    r(0, 2) should be (2)
    r(1, 0) should be (3)
    r(1, 1) should be (4)
    r(1, 2) should be (5)
  }
  
}