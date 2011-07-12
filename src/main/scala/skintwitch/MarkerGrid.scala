package skintwitch

import scala.collection.immutable._
import mocaputils.Marker
import math.abs
import skintwitch.mesh.TriMesh

trait MarkerGrid extends Grid[Marker] { self =>
      
  /** Computes the deformation gradient tensor using the Peters1987 method.
   *  
   *  @param s0 un-deformed index
   *  @param s deformed index
   *  @return new grid with computed deformation gradient tensor */
  def dgtensor(s0: Int, s: Int): Grid[Mat3] = 
    new Grid[Mat3]{
      val numRows = self.numRows
      val numCols = self.numCols
      def apply(row: Int, col: Int) = {
        val adj = self.getFullAdjacent(row, col)
        val qu = adj.map(rc => self(rc._1, rc._2).co(s0)).map(Vec3(_))
        val qd = adj.map(rc => self(rc._1, rc._2).co(s)).map(Vec3(_))
        TensorUtils.dgtensor(self(row, col).co(s0), qu,
                             self(row, col).co(s), qd)
      }
    }
  
  /** Computes the gradient (rate) of the deformation gradient tensor over
   *  the grid using the Peters1987 method.
   *  
   *  @param sample sample at which to compute the tensor
   *  @return new grid of the computed tensor */
  def dgtensorRate(sample: Int): Grid[Mat3] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    dgtensor(s0, sample).map(_ / period)
  }
  
  /** Computes the Biot strain tensor using the Peters1987 method. */
  def biot(s0: Int, s: Int): Grid[Mat3] = {
    synchronized {
      biotCache.getOrElse((s0, s), {
        val b = biot_worker(s0, s)
        biotCache((s0, s)) = b
        b
      })
    }
  }
  private val biotCache =
    scala.collection.mutable.Map.empty[(Int, Int), Grid[Mat3]]
  private def biot_worker(s0: Int, s: Int): Grid[Mat3] = {
    val fGrid = dgtensor(s0, s)
    fGrid.map(e => {
      val (r, u) = e.polar
      u - Mat3.identity
    })
  }
  
  /** Computes the rate of the Biot strain tensor using the Peters1987 method.
   */
  def biotRate(sample: Int): Grid[Mat3] = {
    synchronized {
      biotRateCache.getOrElse(sample, {
        val pbg = biotRate_worker(sample)
        biotRateCache(sample) = pbg
        pbg
      })
    }
  }
  private val biotRateCache = 
    scala.collection.mutable.Map.empty[Int, Grid[Mat3]]
  private def biotRate_worker(sample: Int): Grid[Mat3] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    biot(s0, sample).map(_ / period)
  }
  
  /** Computes the first invariant of the Left Cauchy-Green deformation
   *  tensor.  This is equivalent to the sum of the squares of the principal
   *  stretches.
   *  
   *  @param s0 un-deformed (reference) index
   *  @param s deformed index
   *  @return first invariant */
  def lCauchyGreenI1(s0: Int, s: Int): 
  Grid[Double] = {
    dgtensor(s0, s).map { f =>
      // get the principal stretches from the deformation gradient tensor
      val (r, u) = f.polar
      val prin = u.eig
      val stretches = prin.map(_._1)
      
      // one of the principal stretches will be closest to 1.0; we want to
      //  get rid of this one (it's the artificial, out-of-plane stretch,
      //  which we couldn't calculate, so we set it to one).  we'll replace it
      //  with a stretch which makes the material incompressible (isochoric).
      //  if the stretches are so uniformly close to 1.0 that we accidentally 
      //  pick the incorrect stretch to adjust, then it won't make much of
      //  a difference numerically, since it will be adjusted back close to 
      //  1.0 anyway.
      val sToOne = stretches.map((x: Double) => abs(x - 1.0))
      val compStretches = (stretches zip sToOne).sortBy(_._2).tail.map(_._1)
      assert(compStretches.length == 2)
      val (l1, l2) = (compStretches(0), compStretches(1))
      val l3 = 1.0 / (l1 * l2)
      
      // finally compute the first invariant of the left Cauchy-Green tensor
      //  (which is the sum of the squares of the principal stretches)
      l1 * l1 + l2 * l2 + l3 * l3
    }
  }
  
  /** Computes the average first invariant of the left Cauchy Green deformation
   *  tensor over the grid.  This is equivalent to the average sum of the
   *  squares of the principal stretches.
   *  
   *  @param s0 un-deformed (reference) index
   *  @param s deformed index
   *  @return average first invariant */
  def avgLCauchyGreenI1(s0: Int, s: Int): Double = {
    val i1Grid = lCauchyGreenI1(s0, s)
    val i1Sum = (for {
      row <- 0 until i1Grid.numRows
      col <- 0 until i1Grid.numCols
    } yield i1Grid(row, col)).sum
    i1Sum / (i1Grid.numRows * i1Grid.numCols).toDouble
  }
  
  /** Finds the average position (unweighted centroid) of the markers at a 
   *  given time sample.
   *  
   *  @param s sample at which to compute the average position
   *  @return average position at sample s */
  def avgPosition(s: Int): (Double, Double, Double) = {
    val rm = rowMajor
    val xs = rm.map(_.xs(s))
    val ys = rm.map(_.ys(s))
    val zs = rm.map(_.zs(s))
    val nMarkers = numRows * numCols
    (xs.sum / nMarkers, ys.sum / nMarkers, zs.sum / nMarkers)
  }
  
  /** Average position (unweighted centroid) of the markers of the grid over
   *  all time. */
  lazy val avgPosition: (Double, Double, Double) = {
    val nSamples = self(0, 0).co.length
    val allSamples = (0 until nSamples).par.map(avgPosition(_))
    val xs = allSamples.map(_._1)
    val ys = allSamples.map(_._2)
    val zs = allSamples.map(_._3)
    (xs.sum / nSamples, ys.sum / nSamples, zs.sum / nSamples)
  }

  /** Dices the marker grid to a triangular mesh at a given sample time.
   * 
   *  @param index sample point at which to take marker coordinates
   *  @return triangular mesh at the specified sample time */  
  def diceToTrimesh(index: Int): TriMesh = {
    synchronized { // protect against multiple-thread access
      triMeshCache.getOrElse(index, {
        val triMesh = diceToTrimesh_worker(index)
        triMeshCache(index) = triMesh
        triMesh
      })
    }
  }  
  private val triMeshCache = scala.collection.mutable.Map.empty[Int, TriMesh]
  private def diceToTrimesh_worker(index: Int): TriMesh = {
    // compute vertices
    val verts = rowMajor.map(_.co(index)).toIndexedSeq
    // compute triangular faces
    def rowColToIndex(rc: (Int, Int)) = rc._2 + rc._1 * numCols
    val faceBuilder = new VectorBuilder[(Int, Int, Int)]
    faceBuilder.sizeHint(2 * (numRows - 1) * (numCols - 1))
    for {
      row <- 0 until numRows - 1
      col <- 0 until numCols - 1
    } {
      val triASeq = Vector(
        (0 + row, 0 + col),
        (1 + row, 1 + col),
        (0 + row, 1 + col)).map(rowColToIndex(_))
      val triBSeq = Vector(
        (0 + row, 0 + col),
        (1 + row, 0 + col),
        (1 + row, 1 + col)).map(rowColToIndex(_))
      val triA = (triASeq(0), triASeq(1), triASeq(2))
      val triB = (triBSeq(0), triBSeq(1), triBSeq(2))
      faceBuilder += triA
      faceBuilder += triB
    }
    // compute texture coordinates: u goes along the columns of the grid,
    //  while v goes along the rows
    val texCoords = for {
      row <- 0 until numRows
      col <- 0 until numCols
      s = row / (numRows - 1).toDouble
      t = col / (numCols - 1).toDouble
    } yield (s, t)
    // return the build mesh
    new TriMesh(verts, faceBuilder.result, Some(texCoords))
  }
  
  /** Computes the Biot strain tensor in 2D, over the surface of the grid. 
   *
   *  The tensor is expressed in `(u, v)` parametric coordinates, where
   *  `u` increases from 0 to 1 along the columns of the grid, and `v`
   *  increases from 0 to 1 along the rows of the grid.
   *
   *  @param s0 un-deformed sample
   *  @param s deformed sample
   *  @return 2D Biot strain tensor */
  def biot2D(s0: Int, s: Int): Grid[Mat2] = {
    // first compute the 3D biot tensor grid
    val b3 = biot(s0, s)
    // now transform to 2D according using a new grid
    new Grid[Mat2] {
      val numRows = self.numRows
      val numCols = self.numCols
      def apply(row: Int, col: Int): Mat2 = {
        TensorUtils.projectTensorTo2D(normal(row, col), 
                                      uvec(row, col),
                                      vvec(row, col),
                                      b3(row, col))
      }
      private def uvec(row: Int, col: Int): Vec3 = {
        val (c0, c1) = if (col < numCols - 1) (col, col + 1)
                       else (col - 1, col)
        val x0 = self(row, c0).co(s0)
        val x1 = self(row, c1).co(s0)
        Vec3(x1._1 - x0._1, x1._2 - x0._2, x1._3 - x0._3)
      }
      private def vvec(row: Int, col: Int): Vec3 = {
        val (r0, r1) = if (row < numRows - 1) (row, row + 1)
                       else (row - 1, row)
        val x0 = self(r0, col).co(s0)
        val x1 = self(r1, col).co(s0)
        Vec3(x1._1 - x0._1, x1._2 - x0._2, x1._3 - x0._3)
      }
      private def normal(row: Int, col: Int): Vec3 = {
        val adj = self.getFullAdjacent(row, col)
        val q = adj.map(rc => self(rc._1, rc._2).co(s0)).map(Vec3(_))
        val p = self(row, col).co(s0)
        TensorUtils.calcNormal(p, q)
      }
    }
  }
  
}

object MarkerGrid {

  /** Constructs a `MarkerGrid` from a sequence of specially-named markers.
   *  
   *  Each marker which will form part of the grid should be named as
   *  `CxRy`, where `x` is the column, and `r` is the row.
   *  
   *  @param markers sequence of markers
   *  @return marker grid */
  def fromCRMarkers(markers: Seq[Marker]): MarkerGrid = {
    // find all markers which match the CxRy pattern, and put them into a
    //  sequence of ColRowMarker case classes
    case class ColRowMarker(col: Int, row: Int, marker: Marker)
    val CRTemplate = """C(\d+)R(\d+)""".r
    val crMarkers = for {
      m <- markers
      crmOption = m.name match {
        case CRTemplate(c, r) => Some(ColRowMarker(c.toInt, r.toInt, m))
        case _ => None
      }
      if (crmOption.isDefined)
    } yield crmOption.get
    
    // find min/max col and min/max row
    val cols = crMarkers.map(_.col)
    val rows = crMarkers.map(_.row)
    require(!cols.isEmpty && !rows.isEmpty, 
        "some row or column markers must be present")
    val (minCol, maxCol) = (cols.min, cols.max)
    val (minRow, maxRow) = (rows.min, rows.max)

    // populate an anonymous grid which looks-up rows and columns
    val lookupGrid = new MarkerGrid {
      val numRows = maxRow - minRow + 1
      val numCols = maxCol - minCol + 1
      def apply(row: Int, col: Int) = crMarkers.
        find(m => m.row == row + minRow && m.col == col + minCol).get.marker
    }
    
    // convert to a wrapped VGrid (for faster access: should be faster lookup)
    new MarkerGrid {
      private val vgrid = VGrid(lookupGrid)
      val numRows = vgrid.numRows
      val numCols = vgrid.numCols
      def apply(row: Int, col: Int) = vgrid(row, col)
      override def map[Z](f: Marker => Z) = vgrid.map(f)
    }
  }

}