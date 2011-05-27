package skintwitch

import org.jfree.chart.StandardChartTheme
import java.awt.Color
import org.jfree.chart.plot.XYPlot
import org.jfree.ui.RectangleInsets
import org.jfree.chart.JFreeChart

class WhiteChartTheme extends StandardChartTheme("White Chart Theme") {
  
  private val plotBackground = Color.WHITE
  private val gridLineColor = new Color(0, 0, 0, 40)
  
  override def applyToXYPlot(plot: XYPlot) {
    
    // perform super-class application
    super.applyToXYPlot(plot)
    
    // set plot background
    plot.setBackgroundPaint(plotBackground)
    
    // set gridline colors
    plot.setRangeGridlinePaint(gridLineColor)
    plot.setDomainGridlinePaint(gridLineColor)
    
    // set axes to a style in which they are connected directly to the plot
    plot.setAxisOffset(RectangleInsets.ZERO_INSETS)
    plot.getDomainAxis().setAxisLineVisible(false)
    plot.getRangeAxis().setAxisLineVisible(false)
    
  }
  
}

/**
 * Companion object for WhiteChartTheme.
 * This can be applied to a chart: JFreeChart as:
 *     WhiteChartTheme(chart)
 */
object WhiteChartTheme {
  private lazy val themeInstance = new WhiteChartTheme
  def apply(chart: JFreeChart) = themeInstance.apply(chart)
}

