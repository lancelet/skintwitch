package skintwitch.analysis

import scala.collection.immutable._
import skintwitch.Mat2
import skintwitch.Grid

class Averaging

object Averaging {

  def mean(points: Seq[(Double, Double)]): (Double, Double) = {
    val (x, y) = 
      points.reduce((pta: (Double, Double), ptb: (Double, Double)) => {
        (pta._1 + ptb._1, pta._2 + ptb._2)
      })
    (x / points.length.toDouble, y / points.length.toDouble)
  }
  
  def mean(points: Seq[Option[(Double, Double)]]): Option[(Double, Double)] = {
    val defPts = points.filter(_.isDefined).map(_.get)
    if (defPts.length > 0) {
      Some(mean(defPts))
    } else {
      None
    }
  }
  
  def mean(matrices: Seq[Mat2]): Mat2 = 
    matrices.reduce(_ + _) / matrices.length.toDouble
  
  def mean(grids: Seq[Grid[Mat2]]): Grid[Mat2] = new Grid[Mat2] {
    val numRows = grids.head.numRows
    val numCols = grids.head.numCols
    assert(grids.forall(_.numRows == numRows))
    assert(grids.forall(_.numCols == numCols))
    def apply(row: Int, col: Int): Mat2 = mean(grids.map(_(row, col)))
  }
  
  def meanGridDouble(grids: Seq[Grid[Double]]): Grid[Double] = 
    new Grid[Double] {
      val numRows = grids.head.numRows
      val numCols = grids.head.numCols
      assert(grids.forall(_.numRows == numRows))
      assert(grids.forall(_.numCols == numCols))
      private val nGrids: Float = grids.length.toFloat
      def apply(row: Int, col: Int): Double = 
        grids.map(_(row, col)).sum / nGrids
    }
  
  /** Returns the coordinates of the maximum value of a grid of Doubles. */
  def maxCoords(grid: Grid[Double]): (Int, Int) = {
    var rowMax = 0
    var colMax = 0
    var curMax = grid(rowMax, colMax)
    for {
      row <- 0 until grid.numRows
      col <- 0 until grid.numCols
    } {
      if (grid(row, col) > curMax) {
        curMax = grid(row, col)
        rowMax = row
        colMax = col
      }
    }
    (rowMax, colMax)
  }
  
}
