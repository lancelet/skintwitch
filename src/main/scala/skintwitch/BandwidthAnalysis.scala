package skintwitch

import java.io.{ File, FilenameFilter, FileWriter }
import scala.collection.immutable._
import scala.util.logging.{ ConsoleLogger, Logged }
import mocaputils.{ GapFiller, Marker, TRCReader }
import mocaputils.plotting.{ XYDataset, XYBinnedDataset, StaticDataset }
import mocaputils.plotting.BinnedXYSeries
import signal.PSD
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.ChartPanel
import scala.swing.Frame
import signal.Detrend
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import mocaputils.plotting.XYSeries
import signal.Butter
import signal.FiltFilt
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.renderer.xy.XYShapeRenderer
import java.awt.geom.Ellipse2D
import java.awt.Color
import org.jfree.ui.RectangleAnchor
import org.jfree.ui.RectangleEdge
import mocaputils.plotting.PlotToPDF
import mocaputils.plotting.TimeSampledSeries
import org.jfree.chart.plot.DatasetRenderingOrder
import java.io.Writer
import mocaputils.GappedMarker

/** Bandwidth Analysis of Trials
 * 
 *  This works, but it's quite rough: some re-factoring work is definitely
 *  needed! */
class BandwidthAnalysis extends Logged {

  // banner
  log("+--------------------+")
  log("| Bandwidth Analysis |")
  log("+--------------------+")  
  
  // plotting colors
  val xColor = new Color(0.5f, 0.0f, 0.0f)
  val yColor = new Color(0.0f, 0.5f, 0.0f)
  val zColor = new Color(0.0f, 0.0f, 0.5f)
  val lxColor = new Color(0.8f, 0.6f, 0.6f)
  val lyColor = new Color(0.6f, 0.8f, 0.6f)
  val lzColor = new Color(0.6f, 0.6f, 0.8f)
  
  // makes a color transparent
  def transparent(c: Color, alpha: Double = 0.5) = {
    val r = c.getRGBComponents(null)
    new Color(r(0), r(1), r(2), alpha.toFloat)
  }
  
  // format a string for TeX
  def sTeX(s: String): String = {
    s.replaceAllLiterally("_", "\\_")
  }
  
  // data directory and data files
  val dataDir = "./data"
  val dataFileNames: Seq[String] = (new File(dataDir)).listFiles(
    new FilenameFilter {
      def accept(dir: File, name: String) = name.toLowerCase.endsWith(".trc")
    }
  ).map(_.getCanonicalFile.getPath).toList
  
  // output directory and file
  val outDir = "./output"
  val writer = new FileWriter("%s/tex/bandwidth.tex" format outDir)
  
  // process each file
  for (fileName <- dataFileNames) {
    log("Reading file: \"%s\"" format fileName)
    val trc = TRCReader.read(fileName).fold(
      e => log("Could not read file; message: %s" format e),
      s => {
        val trialName = (new File(fileName)).getName.dropRight(4)
        writer.write("\\section{Trial %s}\n" format sTeX(trialName))
        writer.write("The trial data file name was ``\\texttt{%s.trc}''.\n". 
            format (sTeX(trialName)))
        writer.write("Marker positions were collected at a " +
            "sampling frequency of " +
            "$F_s=%.1f\\,\\mathrm{Hz}$.\n".format(s.cameraRate))
        
        // force-fill any gaps in the markers
        val filledMarkers = s.markers.map(GapFiller.fillGapsLerp(_).get)
        // find bandwidth of all markers (sorted largest to smallest)
        val bandwidths = filledMarkers.map(m => (m, m.bandwidth(0.9))).
            sortWith((x, y) => y._2 < x._2)

        // export bandwidth information
        saveBandwidthTable(writer, bandwidths, trialName)
            
        // export gap information
        saveGapTable(writer, s.markers, trialName)
                
        // 90%ile marker
        val bm90 = bandwidths((bandwidths.length * (1.0 - 0.9)).toInt)
        
        // plot marker psd
        plotPSD(bm90._1, bm90._2, trialName)
                    
        // plot deviation of marker from mean position, along with filtered
        //  version
        plotFilteredComparison(bm90._1, bm90._2, trialName)
        
        // export image links
        saveImageLinks(writer, trialName, bm90._2 * 8, bm90._1)      
      }
    )
  }

  writer.close

  private def saveImageLinks(w: Writer, trialName: String, Fc: Double,
      marker: Marker) {
    val psdShort = "PSD for %s.".format(sTeX(trialName))
    val psdCaption = ("Power spectral density plot for trial %s, "
      + "90th percentile marker %s.").
      format(sTeX(trialName), marker.name)
    val filteredShort = "Filtered coordinates for %s.".format(sTeX(trialName))
    val filteredCaption = "Marker coordinates (relative to the mean) " +
      "of 90th percentile marker %s, ".format(marker.name) +
      "filtered using a 2nd order forward-reverse low-pass " +
      "Butterworth filter, with a cutoff frequency of " +
      "$F_c=%.1f\\,\\mathrm{Hz}$.".format(Fc)
    
    w.write("\\begin{figure}[H]\n")
    w.write("\\includegraphics[width=\\columnwidth]" + 
      "{../output/plots/%s_psd.pdf}" format trialName)
    w.write("\\caption[%s]{%s}\n" format (psdShort, psdCaption))
    w.write("\\end{figure}\n")
    w.write("\\begin{figure}[H]\n")
    w.write("\\includegraphics[width=\\columnwidth]" +
      "{../output/plots/%s_filtered.pdf}" format trialName)
    w.write("\\caption[%s]{%s}\n" format (filteredShort, filteredCaption))
    w.write("\\end{figure}\n")
  }
  
  private def saveBandwidthTable(
      w: Writer,
      bandwidths: Seq[(Marker, Double)],
      trialName: String,
      nCols: Int = 2) {
    
    val nPerCol = math.ceil(bandwidths.length.toDouble / nCols).toInt
    def bws(b: (Marker, Double)) = "%s&%.1f" format (b._1.name, b._2)
    val bw = bandwidths.grouped(nPerCol).map(_.map(bws(_))).
      map(_.padTo(nPerCol, "")).toList.transpose
    val caption = "Marker bandwidth (90\\%% signal power) for trial %s.".
      format(sTeX(trialName))
    val scap = "Bandwidth for %s." format(sTeX(trialName))
      
    w.write("\\begin{longtable}{%s}\n" format ("lr" * nCols))
    w.write("\\toprule\n")
    val mg = List("\\textbf{Marker}", "\\textbf{Bandwidth (Hz)}")
    w.write(latexTableRow(Stream.continually(mg).flatten.take(nCols*2)) + "\n")
    w.write("\\midrule\\endhead\n")
    w.write("\\bottomrule\\caption[]{" + caption +
            "\\\\(Continued on next page\\ldots)}" +
            "\\endfoot\n")
    w.write("\\bottomrule\\caption[" + scap + "]{" + caption + 
      "}\\endlastfoot\n")

    bw.map(b => w.write(latexTableRow(b) + "\n"))
    
    w.write("\\end{longtable}\n")
  }

  private def saveGapTable(w: Writer, markers: Seq[GappedMarker], 
    trialName: String) {
    if (markers.exists(!_.gaps.isEmpty)) {
      val caption = "Gaps for trial %s.".format(sTeX(trialName))
      val scap = "Gaps for %s.".format(sTeX(trialName))
        
      w.write("\\begin{longtable}{lp{8.5cm}}\n")
      w.write("\\toprule\n")
      w.write("\\textbf{Marker} & \\textbf{Gaps (sample, sample)}\\\\\n")
      w.write("\\midrule\\endhead\n")
      w.write("\\bottomrule\\caption[]{" + caption +
          "\\\\(Continued on next page\\ldots)}\\endfoot\n")
      w.write("\\bottomrule\\caption[" + scap + "]{" + caption + 
          "}\\endlastfoot\n")
      for (m <- markers.filterNot(_.gaps.isEmpty)) {
        val gapString = (for (g <- m.gaps) yield "(%d, %d)" format (g._1, g._2)).
          reduce(_ + " " + _)
        w.write("%s & %s\\\\\n" format(m.name, gapString))
      }
      w.write("\\end{longtable}\n")
    }
  }
  
  /** Plots a comparison of original marker position vs filtered position. */
  private def plotFilteredComparison(m: Marker, fc: Double, trialName: String) {
    // take marker coordinates and subtract average over time
    val x = m.xs.map(_ - m.xs.sum / m.xs.length)
    val y = m.ys.map(_ - m.ys.sum / m.ys.length)
    val z = m.zs.map(_ - m.zs.sum / m.zs.length)
    // filter using a 2nd Order Butterworth with 8 x bandwidth
    val butSos = Butter.butterSOSEven(2, 8 * fc / (m.fs / 2))(0)
    val b = List[Double](butSos.b0, butSos.b1, butSos.b2)
    val a = List[Double](1, butSos.a1, butSos.a2)
    val xf = FiltFilt.filtfilt(b, a, x)
    val yf = FiltFilt.filtfilt(b, a, y)
    val zf = FiltFilt.filtfilt(b, a, z)
    // create a dataset for original data
    val dataOriginal = new XYDataset with StaticDataset {
      val series = Vector(
        TimeSampledSeries("x", x), 
        TimeSampledSeries("y", y), 
        TimeSampledSeries("z", z))
    }
    // create a dataset for filtered data
    val dataFiltered = new XYDataset with StaticDataset {
      val series = Vector(
        TimeSampledSeries("x filtered", xf),
        TimeSampledSeries("y filtered", yf),
        TimeSampledSeries("z filtered", zf))
    }
    // plot
    val chart = ChartFactory.createXYLineChart(
      "", "Sample", "Position - Deviation from Mean (mm)", dataOriginal, 
      PlotOrientation.VERTICAL, true, true, false)
    WhiteChartTheme(chart)
    chart.getLegend.setPosition(RectangleEdge.RIGHT)
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.setDataset(1, dataFiltered)
    plot.setDatasetRenderingOrder(DatasetRenderingOrder.FORWARD)
    val dotRenderer = new XYShapeRenderer {
      setDrawOutlines(false)
      val seriesShape = new Ellipse2D.Double(-3, -3, 6, 6)
      setSeriesShape(0, seriesShape)
      setSeriesShape(1, seriesShape)
      setSeriesShape(2, seriesShape)
      setSeriesPaint(0, lxColor)
      setSeriesPaint(1, lyColor)
      setSeriesPaint(2, lzColor)
    }
    val lineRenderer = new XYLineAndShapeRenderer(true, false) {
      setSeriesPaint(0, xColor)
      setSeriesPaint(1, yColor)
      setSeriesPaint(2, zColor)      
    }
    plot.setRenderer(0, dotRenderer)
    plot.setRenderer(1, lineRenderer)
    PlotToPDF.save(new File("%s/plots/%s_filtered.pdf".
        format(outDir, trialName)), chart, 250, 125)
  }
  
  /** Plots power spectral distribution of a given marker. */
  private def plotPSD(m: Marker, bandwidth: Double, trialName: String) = {
    val chart = ChartFactory.createHistogram(
      "", "Frequency (Hz)", "Power", psdDataset(m), PlotOrientation.VERTICAL,
      false, true, false)
    WhiteChartTheme(chart)
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.getDomainAxis.setUpperBound(bandwidth)
    val barRenderer = new XYBarRenderer {
      setShadowVisible(false)
      setBarPainter(new StandardXYBarPainter)
      setSeriesPaint(0, transparent(xColor))
      setSeriesPaint(1, transparent(yColor))
      setSeriesPaint(2, transparent(zColor))     
    }
    plot.setRenderer(0, barRenderer)
    PlotToPDF.save(new File("%s/plots/%s_psd.pdf" format (outDir, trialName)), 
      chart, 250, 125)
  }
  private def psdDataset(m: Marker) = new XYBinnedDataset with StaticDataset {
    case class PSDXYSeries(xx: Seq[Double], fs: Double, name: String) 
    extends BinnedXYSeries {
      private val pd = PSD.psd(Detrend.detrend(xx), fs)
      require (pd.length > 1)
      private val freqWidth = pd(1)._1 - pd(0)._1
      val length = pd.length
      def apply(i: Int) = pd(i)
      override def getStartX(i: Int) = pd(i)._1
      override def getEndX(i: Int) = pd(i)._1 + freqWidth
    }
    val series = Vector(
      PSDXYSeries(m.xs, m.fs, "x"),
      PSDXYSeries(m.ys, m.fs, "y"),
      PSDXYSeries(m.zs, m.fs, "z")
    )
  }
  
  private def latexTableRow(s: Seq[String]): String = 
    s.reduce(_ + " & " + _) + "\\\\"
}

object BandwidthAnalysis {
  def main(args: Array[String]) {
    val bwa = new BandwidthAnalysis with ConsoleLogger
  }
}