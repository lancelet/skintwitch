package skintwitch

import java.awt.Color
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/** Dumps Grid[Double] to an image with a LUT. */
object GridDumper {

  def dump(file: File, grid: Grid[Double],
           pixelsPerGrid: Int = 50,
           lut: LUT = SpectrumLUT, 
           ellipseRad: Double = 2,
           min: Option[Double] = None, max: Option[Double] = None) {    
    // bicubic interpolate the grid to the correct size
    val width = pixelsPerGrid * (grid.numCols - 1)
    val height = pixelsPerGrid * (grid.numRows - 1)
    val bcGrid = BicubicInterpGrid(grid).toGrid(width, height)
    
    // establish the LUT range
    val rmGrid = bcGrid.rowMajor()
    val minLut = min.getOrElse(rmGrid.min)
    val maxLut = max.getOrElse(rmGrid.max)
    require(maxLut >= minLut)
    val range = maxLut - minLut
    
    // map the interpolated grid into the range [0,1] of the LUT
    val rangedGrid = bcGrid.map(x => (x - minLut) / range)
        
    // map the ranged grid to the LUT
    val cGrid: Grid[Color] = rangedGrid.map(lut)
    
    // write the color grid to an image (this could be optimized?)
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      i <- 0 until width
      j <- 0 until height
    } bi.setRGB(i, j, cGrid(i,j).getRGB)
    
    // write circles to the image
    val g2 = bi.getGraphics.asInstanceOf[Graphics2D]
    g2.setColor(Color.BLACK)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                        RenderingHints.VALUE_ANTIALIAS_ON)
    for {
      r <- 0 until grid.numRows
      c <- 0 until grid.numCols
      x = c * pixelsPerGrid - ellipseRad
      y = r * pixelsPerGrid - ellipseRad
    } g2.fill(new Ellipse2D.Double(x, y, ellipseRad * 2, ellipseRad * 2))
    g2.dispose()
    
    ImageIO.write(bi, "png", file)
  }
  
}
