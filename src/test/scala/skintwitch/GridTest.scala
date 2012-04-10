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
    val m = testGrid.subdivide()(
        () => Linearizable.doubleMultiplyableToLinearizable[Double])
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
  
  test("interpUV") {
    val m = new Grid[Double] {
      private val a = Array(Array(0.0, 1.0), Array(2.0, 3.0))
      val numRows = 2
      val numCols = 2
      def apply(row: Int, col: Int) = a(row)(col)
    }
    implicit val dl = {
      () => Linearizable.doubleMultiplyableToLinearizable[Double]
    }
    val eps = 1e-10
    m.interpUV(0, 0) should be (0.0 plusOrMinus eps)
    m.interpUV(1, 0) should be (1.0 plusOrMinus eps)
    m.interpUV(0, 1) should be (2.0 plusOrMinus eps)
    m.interpUV(1, 1) should be (3.0 plusOrMinus eps)
    m.interpUV(0.5, 0) should be (0.5 plusOrMinus eps)
    m.interpUV(0.5, 1) should be (2.5 plusOrMinus eps)
    m.interpUV(0.5, 0.5) should be (1.5 plusOrMinus eps)
  }
  
  test("maxRowCol") { 
    val g = new Grid[Double] {
      private val a = Array(
        Array( 0.0,  1.0, -5.0,  4.0),
        Array( 2.0,  3.0, 10.0, -8.0),
        Array(-1.0,  0.0,  3.0,  2.0),
        Array( 3.0,  5.0,  9.0,  9.5)
      )
      val numRows = 4
      val numCols = 4
      def apply(row: Int, col: Int) = a(row)(col)
    }
    val maxLoc = g.maxRowCol
    maxLoc._1 should be (1)
    maxLoc._2 should be (2)
  }
  
  test("maxUV") {
    val g = new Grid[Double] {
      private val a = Array(
        Array( 0.0,  1.0, -5.0,  4.0),
        Array( 2.0,  3.0, 10.0, -8.0),
        Array(-1.0,  0.0,  3.0,  2.0),
        Array( 3.0,  5.0,  9.0,  9.5)
      )
      val numRows = 4
      val numCols = 4
      def apply(row: Int, col: Int) = a(row)(col)
    }
    val eps = 1e-10
    val maxLoc = g.maxUV
    maxLoc._1 should be ((1.0 / 3.0) plusOrMinus eps)
    maxLoc._2 should be ((2.0 / 3.0) plusOrMinus eps)    
  }
  
}
