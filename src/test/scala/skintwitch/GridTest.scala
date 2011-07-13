package skintwitch

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Linearizable._

class GridTest extends FunSuite with ShouldMatchers {

  test("map") {
    val testGrid = new Grid[Int] {
      private val a = Array(Array(1,2), Array(3,4))
      val numRows = 2
      val numCols = 2
      def apply(row: Int, col: Int) = a(row)(col)
    }
    val m = testGrid.map(_ * 2)
    m.numRows should equal (2)
    m.numCols should equal (2)
    m(0, 0) should equal (2)
    m(0, 1) should equal (4)
    m(1, 0) should equal (6)
    m(1, 1) should equal (8)
  }
  
  test("subdivide") {
    val testGrid = new Grid[Double] {
      private val a = Array(Array(0.0, 1.0), Array(2.0, 3.0))
      val numRows = 2
      val numCols = 2
      def apply(row: Int, col: Int) = a(row)(col)
    }
    val m = testGrid.subdivide()
    val eps = 1e-10
    m.numRows should equal (3)
    m.numCols should equal (3)
    m(0, 0) should be (0.0 plusOrMinus eps)
    m(0, 1) should be (0.5 plusOrMinus eps)
    m(0, 2) should be (1.0 plusOrMinus eps)
    m(1, 0) should be (1.0 plusOrMinus eps)
    m(1, 1) should be (1.5 plusOrMinus eps)
    m(1, 2) should be (2.0 plusOrMinus eps)
    m(2, 0) should be (2.0 plusOrMinus eps)
    m(2, 1) should be (2.5 plusOrMinus eps)
    m(2, 2) should be (3.0 plusOrMinus eps)
  }
    
}
