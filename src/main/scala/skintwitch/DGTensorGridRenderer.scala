package skintwitch

import java.awt.{ BasicStroke, Color, Graphics2D, RenderingHints }
import java.awt.geom.{ Ellipse2D, Line2D }
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scalala.library.Library._
import scalala.operators.Implicits._
import scalala.tensor.dense._

case class DGTensorGridRenderer(grid: DGTensorGrid) {

  val bgColor = Color.WHITE
  val circleRadius = 5.0
  val circleColor = new Color(0.3f, 0.3f, 0.3f)
  
  def render(g: Graphics2D, width: Int, height: Int, s0: Int, s1: Int) {    
    // compute spacing between grid points
    val xSpacing = width / (grid.numCols + 1.0)
    val ySpacing = height / (grid.numRows + 1.0)
    def locx(i: Int) = (i + 1) * xSpacing
    def locy(j: Int) = (j + 1) * ySpacing

    // clear background
    g.setColor(bgColor)
    g.fillRect(-1, -1, width+2, height+2)

    // turn antialiasing on
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                       RenderingHints.VALUE_ANTIALIAS_ON)
    
    // render the grid vertices
    g.setColor(circleColor)
    for (i <- 0 until grid.numCols; x = locx(i)) {
      for (j <- 0 until grid.numRows; y = locy(j)) {
        g.fill(new Ellipse2D.Double(
          x - circleRadius / 2, y - circleRadius / 2,
          circleRadius, circleRadius))
      }
    }
    
    // render the tensor principal directions and stretches
    val scale = 500.0
    for (i <- 0 until grid.numCols; x = locx(i)) {
      for (j <- 0 until grid.numRows; y = locy(j)) {
        // rotate deformation gradient tensor into the grid coordinate system
        val dg = new DGTensor {
          val f = grid(i, j, s0, s1)  // deformation gradient tensor
          val c = f.t * f  // right Cauchy-Green deformation tensor
          val e = (c - DenseMatrix.eye[Double](3)) * 0.5 // Green-Lagrangian strain
          val r = grid.markerGrid.orthoRot(i, j, s0)
          val back = r * e * r.t
          def apply(ii: Int, jj: Int) = back(ii, jj)
        }
        for ((stretch, dir) <- dg.prin) {
          //val strain = stretch - 1
          val strain = stretch // temporarily using Green-Lagrangian strain
          val xprinu = dir(0)
          val yprinu = dir(1)
          val xp = xprinu * scale * strain / 2
          val yp = yprinu * scale * strain / 2
          if (strain < 0) {
            g.setColor(new Color(1.0f, 0.3f, 0.3f))
          } else {
            g.setColor(new Color(0.3f, 0.3f, 1.0f))
          }
          g.setStroke(new BasicStroke(2))
          g.draw(new Line2D.Double(x + xp, y + yp, x - xp, y - yp))
          // TODO: Project directions to 2D
          if (strain < 0) {
            g.setColor(new Color(1.0f, 0.3f, 0.3f, 0.15f))
          } else {
            g.setColor(new Color(0.3f, 0.3f, 1.0f, 0.15f))
          }
          val as = math.abs(strain)
          g.fill(new Ellipse2D.Double(
            x - scale / 2 * as, y - scale / 2 * as,
            scale * as, scale * as))
        }
      }
    }
  }
  
  def saveToPNG(fileName: String, markerSpacing: Int, s0: Int, s1: Int) {
    val width = markerSpacing * (grid.numCols + 1)
    val height = markerSpacing * (grid.numRows + 1)
    val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    val g = img.getGraphics.asInstanceOf[Graphics2D]
    render(g, width, height, s0, s1)
    g.dispose()
    ImageIO.write(img, "png", new File(fileName))
  }
  
}