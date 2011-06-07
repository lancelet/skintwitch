package skintwitch

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

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
  
}