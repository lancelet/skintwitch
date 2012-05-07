package skintwitch.analysis

import scala.collection.immutable._
import mocaputils.plotting.TimeSampledSeries
import mocaputils.plotting.XYDataset
import mocaputils.plotting.StaticDataset
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import skintwitch.WhiteChartTheme
import mocaputils.plotting.PlotToPDF
import java.io.File
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import java.awt.Color
import org.jfree.chart.plot.ValueMarker

object I1Plot {

  def plot(fileName: String, trial: Trial.Result) {
    
    // create time series for the I1 value
    val i1series = TimeSampledSeries("i1", trial.i1Bar, trial.fs)
    // create dataset
    val dataset = new XYDataset with StaticDataset {
      val series = Vector(i1series)
    }
    
    // compute range for the y axis
    val maxI1 = trial.i1Bar.max
    val minI1 = trial.i1Bar.min
    val yRange = maxI1 - minI1
    val padding = 0.1 * yRange
    val maxY = maxI1 + padding
    val minY = minI1 - padding
    
    // create a renderer for the dataset
    val renderer = new XYLineAndShapeRenderer(true, false) {
      setDrawSeriesLineAsPath(true)
      private val solidStroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND,
                                                BasicStroke.JOIN_ROUND)
      setSeriesPaint(0, Color.BLACK)
      setSeriesStroke(0, solidStroke)
    }
    
    // create a marker for the poke time
    val pokeMarker = new ValueMarker(trial.refSample / trial.fs) {
      private val dashStroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND,
                                               BasicStroke.JOIN_ROUND,
                                               1.0f, Array(5.0f, 5.0f),
                                               0.0f)
      setStroke(dashStroke)
    }
    
    // create a marker at the first peak of I1
    val maxResponseMarker = new ValueMarker(trial.maxResponseSample / trial.fs)
    {
      setStroke(new BasicStroke(1.2f))
    }
    
    // create and save the chart
    val chart = ChartFactory.createXYLineChart(
      "", "Time (s)", "Average I1 (stretch squared)", dataset,
      PlotOrientation.VERTICAL, true, true, false)
    WhiteChartTheme(chart)
    chart.removeLegend
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.getRangeAxis.setRange(minY, maxY)
    plot.setRenderer(0, renderer)
    plot.addDomainMarker(pokeMarker)
    plot.addDomainMarker(maxResponseMarker)
    PlotToPDF.save(new File(fileName), chart, 250, 125)
    
  }
  
}