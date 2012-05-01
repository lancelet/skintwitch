package skintwitch.renderplanar

import scala.collection.immutable._
import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Line2D
import skintwitch.Grid
import java.awt.image.BufferedImage
import java.awt.geom.Rectangle2D
import mocaputils.plotting.PlotToPDF
import java.io.File
import skintwitch.Vec2
import skintwitch.Mat2
import java.awt.geom.Ellipse2D
import skintwitch.analysis.Averaging
import java.awt.BasicStroke
import java.awt.geom.Path2D
import skintwitch.Contouring
import skintwitch.Contour

class Render2DTensors  // blah!  apparently Eclipse needs this

/** A chart for Render2DTensors.
 *  
 *  This can be exported using `mocaputils.plotting.PlotToPDF` as follows:
 *  {{{
 *  PlotToPDF.save(file, Render2DTensorsChart(...), width, height)
 *  }}}
 */
case class Render2DTensorsChart(grids: Seq[Grid[Mat2]],
                                stPokes: Seq[Option[Vec2]],
                                stStrokes: Option[Seq[Seq[Vec2]]],
                                renderContours: Boolean,
                                scale: Double = 100) 
{
  def draw(g: Graphics2D, r2d: Rectangle2D) {
    val xform = g.getTransform
    g.translate(r2d.getX, r2d.getY)
    Render2DTensors.render(g, r2d.getWidth, r2d.getHeight, grids, stPokes,
                           stStrokes, renderContours, scale)
    g.setTransform(xform)
  }
}

object Render2DTensors {
  
  /** Renders a plot of 2D tensors on a grid to a PDF file.
   * 
   *  @param fileName name of the file
   *  @param grids sequence of grids of tensors to render
   *  @param stPokes st parametric coordinates of the poke locations
   *  @param stStrokes st parametric coordinates of stroke paths
   *  @param xSpacing grid spacing in the x-direction
   *  @param ySpacing grid spacing in the y-direction
   *  @param scale scale of the tensors (tensor to PDF) */
  def renderToPDF(fileName: String, grids: Seq[Grid[Mat2]],
                  stPokes: Seq[Option[Vec2]],
                  stStrokes: Option[Seq[Seq[Vec2]]],
                  renderContours: Boolean,
                  xSpacing: Int = 10, ySpacing: Int = 10,
                  scale: Double = 100)
  {
    val chart = Render2DTensorsChart(grids, stPokes, stStrokes, renderContours,
                                     scale)
    val width = xSpacing * (grids.head.numCols + 1)
    val height = ySpacing * (grids.head.numRows + 1)
    PlotToPDF.save(new File(fileName), chart, width, height)
  }
  
  /** Renders a plot of 2D tensors on a grid to a BufferedImage.
   * 
   *  @param grids sequence of grids of tensors to render
   *  @param stPokes st parametric coordinates of the poke locations
   *  @param stStrokes st parametric coordinates of stroke paths
   *  @param xSpacing spacing of the grid points in the x (u) direction
   *  @param ySpacing spacing of the grid points in the y (v) direction
   *  @param scale scale of the tensors (tensor to pixel) */
  def renderToBufferedImage(grids: Seq[Grid[Mat2]], 
                            stPokes: Seq[Option[Vec2]],
                            stStrokes: Option[Seq[Seq[Vec2]]],
                            renderContours: Boolean,
                            xSpacing: Int = 50, ySpacing: Int = 50,
                            scale: Double = 100): BufferedImage =
  {
    val width = xSpacing * (grids.head.numCols + 1)
    val height = ySpacing * (grids.head.numRows + 1)
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = bi.createGraphics()
    render(g, width, height, grids, stPokes, stStrokes, renderContours, scale)
    g.dispose()
    bi
  }
  
  /** Renders a plot of 2D tensors on a grid.
   * 
   *  @param g Graphics2D instance to use for drawing
   *  @param width width of the Graphics2D drawing area
   *  @param height height of the Graphics2D drawing area
   *  @param grids sequence of grids of 2D tensors 
   *  @param stPokes st parametric coordinates of the poke locations
   *  @param stStrokes st parametric coordinates of stroke paths
   *  @param scale scale of the tensor */
  def render(g: Graphics2D, width: Double, height: Double, 
             grids: Seq[Grid[Mat2]], stPokes: Seq[Option[Vec2]],
             stStrokes: Option[Seq[Seq[Vec2]]],
             renderContours: Boolean,
             scale: Double = 100) 
  {
    val mean = if (grids.length > 1) {
      Averaging.mean(grids)
    } else {
      grids.head
    }
    
    // compute x and y spacing of the grid
    val xSpacing = width / (mean.numCols + 1).toDouble
    val ySpacing = height / (mean.numRows + 1).toDouble

    // render poke coordinates
    for (stPoke <- stPokes) {
      if(stPoke.isDefined) {
        // find poke coordinates
        val pokeX = stPoke.get.x * xSpacing * (mean.numCols - 1) + xSpacing
        val pokeY = stPoke.get.y * ySpacing * (mean.numRows - 1) + ySpacing
        // plot poke location
        val pokeCircleDiam = xSpacing / 6
        g.setColor(new Color(0.2f, 0.2f, 0.2f, 0.4f))
        g.fill(new Ellipse2D.Double(
          pokeX - pokeCircleDiam / 2, pokeY - pokeCircleDiam / 2,
          pokeCircleDiam, pokeCircleDiam))
      }
    }
    
    // render stroke paths
    if (stStrokes.isDefined) {
      for (stStroke <- stStrokes.get) {
        // transform coordinates
        val coords = stStroke.map((st: Vec2) => {
          val x = st.x * xSpacing * (mean.numCols - 1) + xSpacing
          val y = st.y * ySpacing * (mean.numRows - 1) + ySpacing
          (x, y)
        })
        // create path
        val path = new Path2D.Double
        path.moveTo(coords.head._1, coords.head._2)
        for ((x, y) <- coords) path.lineTo(x, y)
        // stroke path
        g.setColor(new Color(0.2f, 0.2f, 0.2f, 0.4f))
        g.setStroke(new BasicStroke(2.0f))
        g.draw(path)
      }
    }
    
    // plot contours for the grid of tensor invariants
    if (renderContours) {
      def i1(m: Mat2): Double = m.eig.map(_._1).map(x => x * x).sum
      val i1Mean: Grid[Double] = mean.map(i1(_))
      val i1Max = i1Mean.rowMajor.max
      val i1Min = i1Mean.rowMajor.min
      val i1MeanNorm: Grid[Double] = i1Mean.map(
        (x: Double) => (x - i1Min) / (i1Max - i1Min))
      def stToPx(s: Double, t: Double): (Double, Double) = {
        val x = s * xSpacing * (mean.numCols - 1) + xSpacing
        val y = t * ySpacing * (mean.numRows - 1) + ySpacing
        (x, y)
      }
      val contours = {
        var dl = 10
        var contours: Seq[Contour] = null
        while (contours == null) {
          try {
            val i1Interp = Contouring.bicubicInterp(i1MeanNorm, 
                                                    i1MeanNorm.numRows * dl,
                                                    i1MeanNorm.numCols * dl)
            val nContours = 20
            contours = (for {
              i <- 0 until nContours
              x = (i+1).toDouble / (nContours + 2).toDouble
            } yield Contouring.traceContours(i1Interp, x)).flatten
          } catch {
            case ex: AssertionError => dl += 2
          }
          assert(dl <= 50, "Contour division level 50 reached!")
        }
        contours
      }
      for (contour <- contours) {
        val pts = contour.points
        val path = new Path2D.Double
        val (x0, y0) = stToPx(pts.head.x, pts.head.y)
        path.moveTo(x0, y0)
        for (pt <- pts.tail) {
          val (x, y) = stToPx(pt.x, pt.y)
          path.lineTo(x, y)
        }
        if (contour.isClosed) {
          val (x, y) = stToPx(pts.head.x, pts.head.y)
          path.lineTo(x, y)
        }
        g.setColor(new Color(0.0f, 0.0f, 0.0f, 0.5f))
        g.setStroke(new BasicStroke(0.5f))
        g.draw(path)
      }
    }
    
    // plot tensor for all grids at each grid point
    if (grids.length > 1) {
      g.setStroke(new BasicStroke(0.5f))
      for {
        grid <- grids
        row <- 0 until mean.numRows
        col <- 0 until mean.numCols
      } {
        val xform = g.getTransform
        g.translate((col+1) * xSpacing, (row+1) * ySpacing)
        renderTensor(g, grid(row, col), scale, 
            new Color(0.5f, 0.5f, 1.0f), new Color(1.0f, 0.5f, 0.5f))
        g.setTransform(xform)
      }
    }
    
    // plot mean tensor at each grid point
    g.setStroke(new BasicStroke(1.0f))
    for {
      row <- 0 until mean.numRows
      col <- 0 until mean.numCols
    } {
      val xform = g.getTransform
      g.translate((col+1) * xSpacing, (row+1) * ySpacing)
      renderTensor(g, mean(row, col), scale)
      g.setTransform(xform)
    }

  }
  
  /** Renders an individual tensor, centered at (0,0) in the current
   *  coordinate system.
   *
   *  @param g Graphics2D instance to use for drawing
   *  @param tensor the tensor to draw
   *  @param scale scale of the tensor */
  def renderTensor(
      g: Graphics2D, tensor: Mat2, scale: Double,
      compressionColor: Color = Color.BLUE,
      tensionColor: Color = Color.RED
  ) {
    val eigs = tensor.eig
    assert(eigs.length == 2)
    val color1 = if (eigs(0)._1 < 0) compressionColor else tensionColor
    val color2 = if (eigs(1)._1 < 0) compressionColor else tensionColor
    val line1 = new Line2D.Double(
      eigs(0)._2.x * eigs(0)._1 * scale,
      eigs(0)._2.y * eigs(0)._1 * scale,
      -eigs(0)._2.x * eigs(0)._1 * scale,
      -eigs(0)._2.y * eigs(0)._1 * scale
    )
    val line2 = new Line2D.Double(
      eigs(1)._2.x * eigs(1)._1 * scale,
      eigs(1)._2.y * eigs(1)._1 * scale,
      -eigs(1)._2.x * eigs(1)._1 * scale,
      -eigs(1)._2.y * eigs(1)._1 * scale
    )
    g.setColor(color1)
    g.draw(line1)
    g.setColor(color2)
    g.draw(line2)
  }
  
}
