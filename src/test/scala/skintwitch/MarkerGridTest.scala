package skintwitch

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable._
import mocaputils.Marker

class MarkerGridTest extends FunSuite with ShouldMatchers {

  /** Makes a dummy marker sequence with appropriate rows and cols. */
  private def markerSeq(rows: Int, cols: Int): Seq[Marker] = {
    // make a dummy marker with an appropriate row, column name
    def makeMarker(row: Int, col: Int) = new Marker {
      val name = "C%dR%d" format (col, row)
      val co = IndexedSeq.empty[(Double, Double, Double)]
      val fs = 1.0
    }
    // create the marker seq
    for (r <- 0 until rows; c <- 0 until cols) yield makeMarker(r, c)
  }
  
  test("fromCRMarkers") {
    val markers = markerSeq(5, 4)
    val grid = MarkerGrid.fromCRMarkers(markers)
    grid.numRows should be (5)
    grid.numCols should be (4)
    for (r <- 0 until 5; c <- 0 until 4) {
      val name = "C%dR%d" format (c, r)
      grid(r, c).name should equal (name)
    }
  }
  
  test("fromCRMarkers - complain about missing markers") {
    val markers = markerSeq(5, 4).dropRight(1)
    intercept [NoSuchElementException] {
      MarkerGrid.fromCRMarkers(markers)
    }
  }
  
}