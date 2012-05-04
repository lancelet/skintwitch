package skintwitch.analysis

import scala.collection.immutable._
import mocaputils.collection.immutable.RichSeq
import mocaputils.plotting.TimeSampledSeries
import mocaputils.plotting.XYDataset
import mocaputils.plotting.StaticDataset
import mocaputils.plotting.PlotToPDF
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import java.io.File
import skintwitch.WhiteChartTheme
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.Color
import java.awt.BasicStroke
import org.jfree.chart.plot.ValueMarker

object DistancePlot {

  def plot(fileName: String, trial: TrialResult) {
    
    // construct separate series for samples inside the grid and samples
    //  outside the grid
    val distance = trial.distance.map(_.distance)
    val inGrid = trial.distance.map(_.stInGrid())
    val richInGrid = RichSeq(inGrid)
    val inGridRegions = richInGrid.slicesWhere(_ == true)
    val outGridRegions = richInGrid.slicesWhere(_ == false)
    val inGridSeries = for ((start, e) <- inGridRegions) yield {
      val offset = start / trial.fs
      val end = if (e < trial.nSamples) (e + 1) else e
      TimeSampledSeries("inGrid%d" format start,
                        distance.slice(start, end).toIndexedSeq,
                        trial.fs,
                        offset)
    }
    val outGridSeries = for ((start, e) <- outGridRegions) yield {
      val offset = start / trial.fs
      val end = if (e < trial.nSamples) (e + 1) else e
      TimeSampledSeries("outGrid%d" format start,
                        distance.slice(start, end).toIndexedSeq,
                        trial.fs,
                        offset)
    }
    
    // compute the range for the y axis of the plot, based upon the
    //  inGridSeries.  if there are no inGridSeries, then use outGridSeries -
    //  this can happen for control trials.
    val (minY, maxY) = {
      val (mx, mn) = if (!inGridSeries.isEmpty) {
        val maxInGrid = inGridSeries.map(_.x.max).max
        val minInGrid = inGridSeries.map(_.x.min).min
        (maxInGrid, minInGrid)
      } else {
        val maxOutGrid = outGridSeries.map(_.x.max).max
        val minOutGrid = outGridSeries.map(_.x.min).min
        (maxOutGrid, minOutGrid)
      }
      val yRange = mx - mn
      val padding = 0.1 * yRange
      val maxY = mx + padding
      val minY = mn - padding
      (minY, maxY)
    }
    
    // create a new dataset
    val dataset = new XYDataset with StaticDataset {
      val series = Seq(inGridSeries, outGridSeries).flatten.toIndexedSeq
    }
    
    // create a renderer for the dataset
    val renderer = new XYLineAndShapeRenderer(true, false) {
      private val solidStroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND,
                                                BasicStroke.JOIN_ROUND)
      private val dashStroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND,
                                               BasicStroke.JOIN_ROUND,
                                               1.0f, Array(5.0f, 5.0f),
                                               0.0f)
      setDrawSeriesLineAsPath(true)
      for ((s, i) <- inGridSeries.zipWithIndex) {
        setSeriesPaint(i, Color.BLACK)
        setSeriesStroke(i, solidStroke)
      }
      for {
        (s, j) <- outGridSeries.zipWithIndex
        i = inGridSeries.length + j
      } {
        setSeriesPaint(i, Color.BLACK)
        setSeriesStroke(i, dashStroke)
      }
    }
    
    // create a marker at the poke time
    val pokeMarker = new ValueMarker(trial.refSample / trial.fs) {
      setStroke(new BasicStroke(1.2f))
    }
    
    // create and save the chart
    val chart = ChartFactory.createXYLineChart(
        "", "Time (s)", "Distance from marker grid (mm)", dataset,
        PlotOrientation.VERTICAL, true, true, false)
    WhiteChartTheme(chart)
    chart.removeLegend
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.getRangeAxis.setRange(minY, maxY)
    plot.setRenderer(0, renderer)
    plot.addDomainMarker(pokeMarker)
    PlotToPDF.save(new File(fileName), chart, 250, 125)
    
  }
  
}