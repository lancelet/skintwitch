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
  
  test("getConnectedTris") {
    val testGrid = new Grid[Int] {
      private val a = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8),
                            Array(9, 10, 11, 12))
      val numRows = 3
      val numCols = 4
      def apply(row: Int, col: Int) = a(row)(col)
    }
    // corner
    testGrid.getConnectedTris(0, 0) should be (Seq(
      Seq((0, 0), (1, 0), (0, 1))))
    // edge
    testGrid.getConnectedTris(0, 1) should be (Seq(
      Seq((0, 1), (1, 1), (0, 2)),
      Seq((0, 1), (0, 0), (1, 1))))
    // center
    testGrid.getConnectedTris(1, 1) should be (Seq(
      Seq((1, 1), (2, 1), (1, 2)),
      Seq((1, 1), (1, 2), (0, 1)),
      Seq((1, 1), (0, 1), (1, 0)),
      Seq((1, 1), (1, 0), (2, 1))))
  }
  
  test("getConnectedTriElements") {
    val testGrid = new Grid[Int] {
      private val a = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8),
                            Array(9, 10, 11, 12))
      val numRows = 3
      val numCols = 4
      def apply(row: Int, col: Int) = a(row)(col)
    }
    // corner
    testGrid.getConnectedTriElements(0, 0) should be (Seq((1, 5, 2)))
    // edge
    testGrid.getConnectedTriElements(0, 1) should be (Seq(
      (2, 6, 3), (2, 1, 6)))
    // center
    testGrid.getConnectedTriElements(1, 1) should be (Seq(
      (6, 10, 7), (6, 7, 2), (6, 2, 5), (6, 5, 10)))
  }
  
}
