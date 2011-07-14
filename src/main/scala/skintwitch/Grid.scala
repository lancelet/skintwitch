package skintwitch

import scala.collection.immutable._

/** Grid of some type T, over which a function can be mapped point-wise. */
trait Grid[T] { self =>
  
  /** Number of rows in the grid. */
  val numRows: Int
  /** Number of columns in the grid. */
  val numCols: Int
  
  /** Fetch an element from the grid. */
  def apply(row: Int, col: Int): T
  
  /** Map a function point-wise over each element of the grid. */
  def map[Z](f: T => Z): Grid[Z] = new Grid[Z] {
    val numRows = self.numRows
    val numCols = self.numCols
    def apply(row: Int, col: Int): Z = f(self(row, col))
  }
  
  /** All vertices that are adjacent, including diagonal ones.
   *  
   *  Vertices are found which are adjacent to the specified vertex (at the
   *  given `row` and `col`).  The method handles central vertices:
   *  {{{
   *     + + +
   *      \|/
   *     +-o-+
   *      /|\
   *     + + +
   *  }}}
   *  Edge vertices:
   *  {{{
   *    +-o-+
   *     /|\
   *    + + +
   *  }}}
   *  and corner vertices:
   *  {{{
   *    o-+
   *    |\
   *    + +
   *  }}}
   *  
   *  @param row row of the central vertex
   *  @param col column of the central vertex
   *  @return sequence of `(Int, Int)` containing adjacent vertices */
  def getFullAdjacent(row: Int, col: Int): Seq[(Int, Int)] = {
    adjacentCache.getOrElse((row, col), {
      val s = getFullAdjacent_worker(row, col)
      adjacentCache((row, col)) = s
      s
    })
  }
  private val adjacentCache =
    scala.collection.mutable.Map[(Int, Int), Seq[(Int, Int)]]()
  private def getFullAdjacent_worker(row: Int, col: Int): Seq[(Int, Int)] = {
    val template = Seq(
      (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
    val ofsTemplate = template.map(rc => (rc._1 + row, rc._2 + col))
    val validTris = ofsTemplate.filter(rc =>
      rc._1 >= 0 && rc._1 < numRows && rc._2 >= 0 && rc._2 < numCols)
    validTris    
  }
  
  /** Returns the grid in a row-major sequence. */
  def rowMajor(): Seq[T] = {
    val vb = new VectorBuilder[T]
    vb.sizeHint(numRows * numCols)
    for {
      r <- 0 until numRows
      c <- 0 until numCols
    } vb += apply(r, c)
    vb.result
  }
  
  /** Returns the grid in a column-major sequence. */
  def colMajor(): Seq[T] = {
    val vb = new VectorBuilder[T]
    vb.sizeHint(numRows * numCols)
    for {
      c <- 0 until numCols
      r <- 0 until numRows
    } vb += apply(r, c)
    vb.result
  }
  
  /** Expands all edges of the grid by one row or column, using linear
   *  interpolation of the grid values (useful for RenderMan patch
   *  bicubic rendering as Catmull-Rom patches). */
  def expandEdgesByOne()(implicit TtoL: () => Linearizable[T]): Grid[T] =
    expandRows.expandCols
    
  private def expandRows()(implicit TtoL: () => Linearizable[T]): Grid[T] =
  new Grid[T] {
    private val l = TtoL()
    val numRows = self.numRows + 2
    val numCols = self.numCols
    def apply(row: Int, col: Int): T = {
      if (row == 0) {
        l.lin(self(0, col), self(1, col), -1)
      } else if (row == numRows - 1) {
        l.lin(self(self.numRows - 2, col), self(self.numRows - 1, col), 2)
      } else {
        self(row - 1, col)
      }
    }
  }
  
  private def expandCols()(implicit TtoL: () => Linearizable[T]): Grid[T] =
  new Grid[T] {
    private val l = TtoL()
    val numRows = self.numRows
    val numCols = self.numCols + 2
    def apply(row: Int, col: Int): T = {
      if (col == 0) {
        l.lin(self(row, 0), self(row, 1), -1)
      } else if (col == numCols - 1) {
        l.lin(self(row, self.numCols - 2), self(row, self.numCols - 1), 2)
      } else {
        self(row, col - 1)
      }
    }
  }
  
  /** Performs simple subdivision of the grid, using bilinear interpolation
   *  to construct a new row and new column between each existing pair of
   *  rows and columns. */
  def subdivide()(implicit TtoL: () => Linearizable[T]): Grid[T] = 
    VGrid(interpRows.interpCols)
  
  /** Expands rows of the grid, by interpolating a new row between every pair
   *  of existing rows. */
  private def interpRows()(implicit TtoL: () => Linearizable[T]): Grid[T] =
  new Grid[T] {
    private val l = TtoL()
    val numRows = 2 * (self.numRows - 1) + 1
    val numCols = self.numCols
    def apply(row: Int, col: Int): T = {
      if (row % 2 == 0) {
        self(row / 2, col)
      } else {
        val r = (row - 1) / 2
        l.lin(self(r, col), self(r + 1, col), 0.5)
      }
    }
  }
  
  /** Expands columns of the grid, by interpolating a new column between
   *  every pair of existing columns. */
  private def interpCols()(implicit TtoL: () => Linearizable[T]): Grid[T] =
  new Grid[T] {
    private val l = TtoL()
    val numRows = self.numRows
    val numCols = 2 * (self.numCols - 1) + 1
    def apply(row: Int, col: Int): T = {
      if (col % 2 == 0) {
        self(row, col / 2)
      } else {
        val c = (col - 1) / 2
        l.lin(self(row, c), self(row, c + 1), 0.5)
      }
    }
  }
  
  /** Bilinearly interpolates the grid.
   * 
   *  @param u parametric coordinate along the columns of the grid (0.0 -> 1.0)
   *  @param v parametric coordinate along the rows of the grid (0.0 -> 1.0)
   *  @return bilinearly-interpolated value */
  def interpUV(u: Double, v: Double)(implicit TtoL: () => Linearizable[T]): T =
  {
    require(u >= 0 && u <= 1)
    require(v >= 0 && v <= 1)
    val l = TtoL()
    val uu = u * (numCols - 1)
    val vv = v * (numRows - 1)
    val c0 = {
      val c0Cand = math.floor(uu).toInt
      if (c0Cand == numCols - 1) (c0Cand - 1) else c0Cand
    }
    val r0 = {
      val r0Cand = math.floor(vv).toInt
      if (r0Cand == numRows - 1) (r0Cand - 1) else r0Cand
    }
    val uf = uu - c0 * (numCols - 1)
    val vf = vv - r0 * (numRows - 1)
    val c1 = c0 + 1
    val r1 = r0 + 1
    val ci0 = l.lin(self(r0, c0), self(r0, c1), uf)
    val ci1 = l.lin(self(r1, c0), self(r1, c1), uf)
    l.lin(ci0, ci1, vf)
  }
  
}
