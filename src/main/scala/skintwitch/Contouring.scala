package skintwitch

import scala.collection.immutable._
import rman.BicubicPatchMesh

case class Contour(points: Seq[Vec2], isClosed: Boolean)
object Contour {
  val empty: Contour = Contour(Seq.empty[Vec2], false)
}

/** Edge in the grid. */
private sealed trait Edge {

  val grid: Grid[Double]
  val vertCoords: Seq[(Int, Int)]
  lazy val vertValues: Seq[Double] = vertCoords.map(rc => grid(rc._1, rc._2))
  lazy val adjacentCells: Seq[Cell] = Seq.empty[Cell]
  
  def spansContour(x: Double): Boolean = {
    val oneAbove = vertValues.exists(_ >= x)
    val oneBelow = vertValues.exists(_ <= x)
    oneAbove && oneBelow
  }
  
  def intersect(x: Double): Vec2 = {
    assert(spansContour(x))
    val a = (x - vertValues(0)) / (vertValues(1) - vertValues(0))
    val b = 1.0 - a
    Vec2((vertCoords(0)._2 * b + vertCoords(1)._2 * a) / (grid.numCols - 1),
         (vertCoords(0)._1 * b + vertCoords(1)._1 * a) / (grid.numRows - 1))
  }
  
  def nextContourEdge(x: Double, currentCell: Cell): Option[(Edge, Cell)] = {
    assert(adjacentCells.contains(currentCell))
    val nextCells = adjacentCells.filter(_ != currentCell)
    assert(nextCells.length <= 1)
    if (nextCells.isEmpty) {
      None
    } else {
      val nextCell = nextCells.head
      val nextEdges = nextCell.spanningEdges(x).filter(_ != this)
      if (nextEdges.length == 0) {
        None
      } else {
        assert(nextEdges.length == 1,
          "More than one edge indicates that a finer grid is required")
        Some((nextEdges.head, nextCell))
      }
    }
  }
  
  def traceContourEdges(
    x: Double, contour: Contour, currentCell: Cell, visitedCells: Seq[Cell]
  ):
  (Contour, Seq[Cell]) = {
    assert(adjacentCells.contains(currentCell))
    assert(spansContour(x))
    
    // add the intersection of this edge to the list of points, and add the
    //  current cell to the list of visited cells
    val points = contour.points :+ intersect(x)
    val cells  = visitedCells :+ currentCell
    
    // if both of the cells connected to this edge are already in the set
    //  of visited cells then we have completed a closed contour
    if (adjacentCells.forall(visitedCells.contains(_))) {
      return (new Contour(points, true), cells)
    }
    
    // find the edge we can follow to expand the contour
    val optNext = nextContourEdge(x, currentCell)

    // if there are no follow-able edges, then we have reached the end of
    //  an open contour
    if (optNext.isDefined == false) {
      return (new Contour(points, false), cells)
    }

    // continue to trace the contour
    val (nextEdge, nextCell) = optNext.get
    nextEdge.traceContourEdges(x, Contour(points, false), nextCell, cells)
  }
}

/** Edge which spans from `(row, col)` to `(row + 1, col)`. */
private case class RowEdge(grid: Grid[Double], row: Int, col: Int) 
extends Edge {
  val vertCoords = Seq((row, col), (row + 1, col))
  override lazy val adjacentCells: Seq[Cell] = {
    val buffer = new scala.collection.mutable.ListBuffer[Cell]()
    if (col > 0) buffer += Cell(grid, row, col - 1)
    if (col < (grid.numCols - 1)) buffer += Cell(grid, row, col)
    buffer.toList
  }
}
/** Edge which spans from `(row, col)` to `(row, col + 1)`. */
private case class ColEdge(grid: Grid[Double], row: Int, col: Int) 
extends Edge {
  val vertCoords = Seq((row, col), (row, col + 1))
  override lazy val adjacentCells: Seq[Cell] = {
    val buffer = new scala.collection.mutable.ListBuffer[Cell]()
    if (row > 0) buffer += Cell(grid, row - 1, col)
    if (row < (grid.numRows - 1)) buffer += Cell(grid, row, col)
    buffer.toList
  }
}

/** Cell in the grid. */
private case class Cell(grid: Grid[Double], row: Int, col: Int) {
  
  lazy val vertCoords: Seq[(Int, Int)] = Seq((row, col),
                                             (row, col + 1),
                                             (row + 1, col),
                                             (row + 1, col + 1))
  lazy val vertValues: Seq[Double] = vertCoords.map(rc => grid(rc._1, rc._2))
  lazy val edges: Seq[Edge] = Seq(RowEdge(grid, row, col),
                                  RowEdge(grid, row, col + 1),
                                  ColEdge(grid, row, col),
                                  ColEdge(grid, row + 1, col))
  
  def spansContour(x: Double): Boolean = {
    val someAbove = vertValues.exists(_ >= x)
    val someBelow = vertValues.exists(_ <= x)
    someAbove && someBelow
  }
  
  def spanningEdges(x: Double): Seq[Edge] = edges.filter(_.spansContour(x))

}

private case class ContourGrid(grid: Grid[Double]) {
  def getCell(row: Int, col: Int): Cell = Cell(grid, row, col)
  lazy val cells: Set[Cell] = {
    val cellSeq = for {
      row <- 0 until (grid.numRows - 1)
      col <- 0 until (grid.numCols - 1)
    } yield getCell(row, col)
    cellSeq.toSet
  }
}

object Contouring {
  
  def traceContours(grid: Grid[Double], level: Double): Seq[Contour] = {
    // fetch a set of all cells in the grid
    val cellSet = scala.collection.mutable.Set.empty[Cell] ++ 
                  ContourGrid(grid).cells.filter(_.spansContour(level))
    val buffer = new scala.collection.mutable.ListBuffer[Contour]
    
    // trace over all cells which span the contour, constructing connected
    //  contour lines
    while (!cellSet.isEmpty) {
      val startCell = cellSet.head
      val spanningEdges = startCell.spanningEdges(level)
      val startEdge = spanningEdges.head
      val (contour, visitedCells) = 
        startEdge.traceContourEdges(level, Contour.empty, startCell, 
                                    Seq.empty[Cell])
      if (contour.isClosed || spanningEdges.length == 1) {
        cellSet --= visitedCells
        buffer += contour
      } else {
        assert(spanningEdges.length == 2)
        val startEdge2 = spanningEdges.tail.head
        val (contour2, visitedCells2) = 
          startEdge2.traceContourEdges(level, Contour.empty, startCell,
                                       Seq.empty[Cell])
        cellSet --= visitedCells
        cellSet --= visitedCells2
        val joinedContour = Contour(
          contour2.points.reverse ++ contour.points, false)
        buffer += joinedContour
      }
    }
    buffer.toList
  }
  
  def bicubicInterp(grid: Grid[Double], numRows: Int, numCols: Int): 
  Grid[Double] =
  {
    val eGrid = grid.expandEdgesByOne()(
        () => Linearizable.doubleMultiplyableToLinearizable[Double])
    val p = (for {
      row <- 0 until eGrid.numRows
      col <- 0 until eGrid.numCols
      z = eGrid(row, col)
    } yield {
      Seq(col.toDouble, row.toDouble, z)
    }).flatten
    val patchMesh = new BicubicPatchMesh(BicubicPatchMesh.catmullRom, 1,
                                         BicubicPatchMesh.catmullRom, 1,
                                         eGrid.numCols, eGrid.numRows, p)
    val nr = numRows
    val nc = numCols
    val newGrid = new Grid[Double] {
      val numRows = nr
      val numCols = nc
      def apply(row: Int, col: Int) = {
        val v = row.toDouble / (numRows - 1).toDouble
        val u = col.toDouble / (numCols - 1).toDouble
        val z = patchMesh(u, v).p.e2
        z
      }
    }
    VGrid(newGrid)
  }
  
}
