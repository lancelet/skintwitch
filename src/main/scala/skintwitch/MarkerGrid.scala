package skintwitch

import scala.collection.immutable._
import mocaputils.Marker

trait MarkerGrid extends Grid[Marker]

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
    val (minCol, maxCol) = (cols.min, cols.max)
    val (minRow, maxRow) = (rows.min, rows.max)

    // populate an anonymous grid which looks-up rows and columns
    val lookupGrid = new MarkerGrid {
      val numRows = maxRow - minRow + 1
      val numCols = maxCol - minCol + 1
      def apply(row: Int, col: Int) = crMarkers.
        find(m => m.row == row - minRow && m.col == col - minCol).get.marker
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