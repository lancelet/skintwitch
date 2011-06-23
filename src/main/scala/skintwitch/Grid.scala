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

  /** Get triangles from the grid which are connected to a specific vertex.
   *  
   *  Triangles connected to a vertex are found by sub-dividing the grid
   *  squares which are adjacent to a given vertex; as shown in this 
   *  ASCII-art figure:
   *  {{{
   *      GRID     4 TRIS FOR VERTEX o    
   *      +-+-+            +  
   *      | | |         2 /|\ 1
   *      +-o-+          +-o-+
   *      | | |         3 \|/ 4
   *      +-+-+            +
   *  }}}
   *  This method also copes with edge and corner vertices:
   *  {{{
   *      EDGE   2 TRIS FOR VERTEX o
   *      +-o-+       +-o-+
   *      | | |      1 \|/ 2
   *      +-+-+         +
   *  }}}
   *  {{{
   *      CORNER   1 TRI FOR VERTEX o
   *        o-+         o-+
   *        | |         |/ 1
   *        +-+         +
   *  }}}
   *  
   *  @param row row of the grid for the vertex of interest
   *  @param col column of the grid for the vertex of interest
   *  @return sequence of sequences of `(row, col)` indices */
  def getConnectedTris(row: Int, col: Int): Seq[Seq[(Int, Int)]] = {
    val template = Seq(
      Seq((0, 0), ( 1,  0), ( 0,  1)),
      Seq((0, 0), ( 0,  1), (-1,  0)),
      Seq((0, 0), (-1,  0), ( 0, -1)),
      Seq((0, 0), ( 0, -1), ( 1,  0))
    )
    val ofsTemplate = template.map(_.map(rc => (rc._1 + row, rc._2 + col)))
    val validTris = ofsTemplate.filter(_.forall(rc =>
      rc._1 >= 0 && rc._1 < numRows && rc._2 >= 0 && rc._2 < numCols))
    validTris
  }

  /** Gets elements from the grid connected by triangles around the target
   *  vertex.  This method collects the elements at the vertices returned by
   *  the `getConnectedTris` method.
   *  
   *  @see getConnectedTris
   *  
   *  @param row row of the grid for the vertex
   *  @param col column of the grid for the vertex of interest
   *  @return sequence of `(T, T, T)` elements from the tris of the grid */
  def getConnectedTriElements(row: Int, col: Int): Seq[(T, T, T)] = {
    val tris = getConnectedTris(row, col)
    assert(tris.forall(_.length == 3))
    tris.map(tri =>
      (apply(tri(0)._1, tri(0)._2),
       apply(tri(1)._1, tri(1)._2),
       apply(tri(2)._1, tri(2)._2)))
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
    val template = Seq(
      (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
    val ofsTemplate = template.map(rc => (rc._1 + row, rc._2 + col))
    val validTris = ofsTemplate.filter(rc =>
      rc._1 >= 0 && rc._1 < numRows && rc._2 >= 0 && rc._2 < numCols)
    validTris
  }
  
}
