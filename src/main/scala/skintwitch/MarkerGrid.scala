package skintwitch

import scala.collection.immutable._
import mocaputils.Marker
import scalala.tensor.Matrix
import scalala.tensor.dense.DenseMatrix
import math.abs
import skintwitch.mesh.TriMesh

trait MarkerGrid extends Grid[Marker] { self =>
      
  /** Computes the deformation gradient tensor using the Peters1987 method.
   *  
   *  @param s0 un-deformed index
   *  @param s deformed index
   *  @return new grid with computed deformation gradient tensor */
  def dgtensor(s0: Int, s: Int): Grid[Matrix[Double]] = 
    new Grid[Matrix[Double]]{
      val numRows = self.numRows
      val numCols = self.numCols
      def apply(row: Int, col: Int) = {
        val adj = self.getFullAdjacent(row, col)
        val qu = adj.map(rc => self(rc._1, rc._2).co(s0))
        val qd = adj.map(rc => self(rc._1, rc._2).co(s))
        TensorUtils.dgtensor(self(row, col).co(s0), qu,
                             self(row, col).co(s), qd)
      }
    }
  
  /** Computes the gradient (rate) of the deformation gradient tensor over
   *  the grid using the Peters1987 method.
   *  
   *  @param sample sample at which to compute the tensor
   *  @return new grid of the computed tensor */
  def dgtensorRate(sample: Int): Grid[Matrix[Double]] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    dgtensor(s0, sample).map(_ / period)
  }
  
  /** Computes the Biot strain tensor using the Peters1987 method. */
  def biot(s0: Int, s: Int): Grid[Matrix[Double]] = {
    val fGrid = dgtensor(s0, s)
    fGrid.map(e => {
      val (r, u) = TensorUtils.polarDecomp(e)
      u - DenseMatrix.eye[Double](3)
    })
  }
  
  /** Computes the rate of the Biot strain tensor using the Peters1987 method.
   */
  def biotRate(sample: Int): Grid[Matrix[Double]] = {
    synchronized {
      biotRateCache.getOrElse(sample, {
        val pbg = biotRate_worker(sample)
        biotRateCache(sample) = pbg
        pbg
      })
    }
  }
  private val biotRateCache = 
    scala.collection.mutable.Map.empty[Int, Grid[Matrix[Double]]]
  private def biotRate_worker(sample: Int): Grid[Matrix[Double]] = {
    val period = 1.0 / self(0, 0).fs
    val s0 = if (sample > 0) sample - 1 else 1
    biot(s0, sample).map(_ / period)
  }
  
  /** Computes the strain energy density at each grid point, assuming an
   *  incompressible, Neo-Hookean material.
   *  
   *  The equation used is: `W = C_1 * (I_1 - 3)`, where `I_1` is the sum of
   *  the squares of the principal stretches, and `C_1` is a material constant.
   *  `C_1` is equal to half of the shear modulus `mu`, and for an
   *  incompressible, isotropic material: `mu = E / (2 * (1 + nu))`, where
   *  `E` is the Young's Modulus and `nu = 0.5` is the Poisson's Ratio.
   *  Expanding this relationship gives: `C_1 = E / 6`.
   *  
   *  @param s0 un-deformed index
   *  @param s deformed index
   *  @param e Young's Modulus of the material
   *  @return grid of strain energy density */
  def incompNeoHookeanSED(s0: Int, s: Int, e: Double): 
  Grid[Double] = {
    dgtensor(s0, s).map { f =>
      // get the principal stretches from the deformation gradient tensor
      val (r, u) = TensorUtils.polarDecomp(f)
      val prin = TensorUtils.principalComp(u)
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
      
      // finally compute the strain energy density
      val i1 = l1 * l1 + l2 * l2 + l3 * l3
      val w = (e / 6.0) * (i1 - 3.0)
      w
    }
  }
  
  /** Computes the average strain energy density over all grid points, assuming
   *  an incompressible, Neo-Hookean material.
   *  
   *  @param s0 un-deformed index
   *  @param s deformed index
   *  @param e Young's Modulus of the material
   *  @return average strain energy density */
  def avgIncompNeoHookeanSED(s0: Int, s: Int, e: Double): Double = {
    val sedGrid = incompNeoHookeanSED(s0, s, e)
    val sedSum = (for {
      row <- 0 until sedGrid.numRows
      col <- 0 until sedGrid.numCols
    } yield sedGrid(row, col)).sum
    sedSum / (sedGrid.numRows * sedGrid.numCols).toDouble
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