package skintwitch

import scala.collection.immutable._
import mocaputils.{ GapFiller, Marker, TRCData }
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.tensor.dense._

trait MarkerGrid { self =>
  
  val numRows: Int
  val numCols: Int
  
  def apply(i: Int, j: Int): Marker
  
  /** Triads of possible markers at a given index; handles corners, edges
   *  and central regions correctly. */
  def getMarkerTris(i: Int, j: Int): Seq[(Marker, Marker, Marker)] = {
    // case class for a pair of coordinates
    case class Co(i: Int, j: Int) {
      val valid = (i >= 0) && (j >= 0) && (i < numCols) && (j < numRows)
      lazy val marker = self.apply(i, j)
    }
    // all possible coordinates
    val x0 = Co(i, j)
    val x1 = Co(i + 1, j)
    val x2 = Co(i, j + 1)
    val x3 = Co(i - 1, j)
    val x4 = Co(i, j - 1)
    // all possible topology
    val q1 = List(x0, x1, x2)
    val q2 = List(x0, x2, x3)
    val q3 = List(x0, x3, x4)
    val q4 = List(x0, x4, x1)
    // cull invalid topology based upon coordinate validity
    val validq = List(q1, q2, q3, q4).filter(_.forall(_.valid))
    // convert to sequence of markers
    validq.map(q => (q(0).marker, q(1).marker, q(2).marker))
  }
  
  private implicit def tupleToDenseVector(x: (Double, Double, Double)) =
    DenseVectorCol(x._1, x._2, x._3)  
  
  def iVec(i: Int, j: Int, s: Int): DenseVector[Double] = {
    if (i < numCols - 1) {
      normalize(apply(i + 1, j).co(s) - apply(i, j).co(s), 2)
    } else {
      normalize(apply(numCols - 1, j).co(s) - apply(numCols - 2, j).co(s), 2)
    }
  }
  
  def jVec(i: Int, j: Int, s: Int): DenseVector[Double] = {
    if (j < numRows - 1) {
      normalize(apply(i, j + 1).co(s) - apply(i, j).co(s), 2)
    } else {
      normalize(apply(i, numRows - 1).co(s) - apply(i, numRows - 2).co(s), 2)
    }
  }
  
  def orthoRot(i: Int, j: Int, s: Int): DenseMatrix[Double] = {
    val e1 = iVec(i, j, s)
    val e3 = normalize(cross(e1, jVec(i, j, s)), 2)
    val e2 = -normalize(cross(e1, e3), 2)
    DenseMatrix(e1.toArray, e2.toArray, e3.toArray)
  }
  
}

object MarkerGrid {
  
  def fromTRC(trc: TRCData, fc: Double): MarkerGrid = {
    
    // find markers which match the pattern CxRy, and put them in Mcr objects,
    //  and low-pass filter them
    case class Mcr(c: Int, r: Int, m: Marker)
    val crMarkers = (for (m <- trc.markers) yield {
      val ColRowMarker = """C(\d+)R(\d+)""".r
      m.name match {
        case ColRowMarker(col, row) => 
          Some(Mcr(col.toInt, row.toInt, 
                   GapFiller.fillGapsLerp(m).get.butter2(fc)))
        case _ => None
      }
    }).filter(_.isDefined).map(_.get)
    
    // find min col, max col, min row, max row
    val mincol = crMarkers.map(_.c).min
    val maxcol = crMarkers.map(_.c).max
    val minrow = crMarkers.map(_.r).min
    val maxrow = crMarkers.map(_.r).max
    
    // create a grid of rows
    val gridArray = (for (r <- minrow to maxrow) yield {
      (for (c <- mincol to maxcol) yield {
        crMarkers.find(mcr => (mcr.r == r) && (mcr.c == c)).get.m
      }).toArray
    }).toArray

    // create the marker grid
    new MarkerGrid {
      val numRows = maxrow - minrow + 1
      val numCols = maxcol - mincol + 1
      def apply(i: Int, j: Int) = gridArray(j)(i)
    }
  }
  
}