package skintwitch

import scala.collection.immutable._

/** Immutable grid implementation, backed by a Vector[Vector[T]]. */
class VGrid[T](v: Vector[Vector[T]]) extends Grid[T] {
  val numRows = v.length
  require(numRows > 0)
  val numCols = v(0).length
  require(numCols > 0 && v.forall(_.length == numCols))
  def apply(row: Int, col: Int): T = v(row)(col)
  override def map[Z](f: T => Z): VGrid[Z] = new VGrid(v.map(_.map(f)))
}

object VGrid {
  /** Constructs a VGrid from a Vector[Vector[T]]. */
  def apply[T](v: Vector[Vector[T]]) = new VGrid(v)
  /** Constructs a VGrid from a pre-existing grid. */
  def apply[T](g: Grid[T]): VGrid[T] = {
    val v = new VectorBuilder[Vector[T]]
    v.sizeHint(g.numRows)
    for (row <- 0 until g.numRows) {
      val v2 = new VectorBuilder[T]
      v2.sizeHint(g.numCols)
      for (col <- 0 until g.numCols) {
        v2 += g(row, col)
      }
      v += v2.result
    }
    new VGrid(v.result)
  }
}