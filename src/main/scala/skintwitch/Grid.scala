package skintwitch

/** Grid of some type T, over which a function can be mapped point-wise. */
trait Grid[T] { self =>
  val numRows: Int
  val numCols: Int
  def apply(row: Int, col: Int): T
  def map[Z](f: T => Z): Grid[Z] = new Grid[Z] {
    val numRows = self.numRows
    val numCols = self.numCols
    def apply(row: Int, col: Int): Z = f(self(row, col))
  }
}
