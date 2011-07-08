package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import vtk.vtkActor2D
import skintwitch.MarkerGrid
import mocaputils.VirtualMarker
import scala.actors.Futures
import javax.swing.SwingUtilities
import vtk.vtkDataObject
import vtk.vtkFieldData
import vtk.vtkDoubleArray
import vtk.vtkXYPlotActor
import vtk.vtkTextActor

class I1PlotActor(
  staticMarkers: Seq[Marker], markers: Seq[Marker], grid: MarkerGrid,
  loadCallback: () => Unit, threshold: Double = 10.0, stepback: Int = 15
) extends Animated2DActor {

  import I1PlotActor._
  
  private var sample: Int = 0
  
  private val lblActor = new vtkTextActor {
    GetPositionCoordinate.SetCoordinateSystemToNormalizedViewport
    SetInput("Computing I1 data...")
    //SetPosition(0.01 + 0.3 - GetWidth, 0.05 + 0.3 - GetHeight)
    SetPosition(0.01, 0.05 + 0.15)
    GetProperty.SetColor(0.4, 0.4, 0.4)
  }
  
  private val plotActor = new vtkXYPlotActor {
    //SetYRange()
    SetWidth(0.3)
    ShowReferenceXLineOn
    SetHeight(0.15)
    SetXTitle("")
    SetYTitle("")
    SetXLabelFormat("")
    SetYLabelFormat("")
    SetPosition(0.01, 0.05 + 0.15)
    GetProperty.SetLineWidth(1.5)
    GetProperty.SetColor(0.6, 0.6, 0.6)
    VisibilityOff
  }
  
  def getActors(): Seq[vtkActor2D] = Seq(plotActor, lblActor)
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def update() {
    plotActor.SetReferenceXValue(sample)
  }
  
  private var i1Future = Futures.future {
    calculateI1()
    SwingUtilities.invokeLater(new Runnable {
      def run() = setI1InActor()
    })
  }
  
  private def setI1InActor() {
    assert(SwingUtilities.isEventDispatchThread)
    val dataObject = new vtkDataObject {
      SetFieldData(new vtkFieldData {
        AddArray(new vtkDoubleArray {
          SetJavaArray(i1.toArray)
        })
      })
    }
    plotActor.AddDataObjectInput(dataObject)
    plotActor.VisibilityOn
    update()
    lblActor.SetInput("I1")
    lblActor.SetPosition(0.01 + 0.3 - 0.15, 
                         0.05 + 0.3 - 0.05)
    loadCallback()
  }
  
  /** Returns distance to tip pointer at each sample, and whether or not the
   *  sample is actually within the main grid. */
  private def getDistances(): Seq[(Double, Boolean)] = {
    // construct markers
    def getMarker(name: String) = markers.find(_.name == name).get 
    val mLong = getMarker("long")
    val mMiddle = getMarker("middle")
    val mShort = getMarker("short")
    val mMed = getMarker("med")
    val mTip: Marker = {
      def sm(name: String) = staticMarkers.find(_.name == name).get.co(0)
      val pLong = sm("long")
      val pMiddle = sm("middle")
      val pShort = sm("short")
      val pMed = sm("med")
      val pTip = sm("T6")
      new VirtualMarker("tip", pTip, Seq(
        (pLong, mLong), (pMiddle, mMiddle), (pShort, mShort), (pMed, mMed)))
    }

    def inGrid(st: (Double, Double)): Boolean = { 
      val minm = 0.01
      val maxm = 1.0 - minm
      (st._1 > minm) && (st._1 < maxm) && (st._2 > minm) && (st._2 < maxm)
    }
    
    // find distance to the mesh at each time index
    for {
      i <- 0 until markers(0).co.length
      triMesh = grid.diceToTrimesh(i)
      (dist, xPoint, st) = triMesh.signedDistanceTo(mTip.co(i))
    } yield (dist, inGrid(st))    
  }

  /** Finds the reference sample index. */
  private def s0(): Int = {
    val nSamples = markers(0).co.length
    val crossing = getDistances.filter(_._2).map(_._1).min + threshold
    val firstIndex = getDistances.map(_._1).indexWhere(_ <= crossing)
    val s0cand = firstIndex - stepback
    if (s0cand < 0) {
      0
    } else if (s0cand >= nSamples) {
      nSamples - 1
    } else {
      s0cand
    }
  }
  
  /** Compute average first invariant of left Cauchy-Green deformation 
   *  tensor. */
  private var i1: Seq[Double] = Seq.empty[(Double)]
  private def calculateI1(): Unit = {
    val nSamples = markers(0).co.length
    val sampleZero = s0()
    // compute I1 in parallel (out-of-order)
    val i1Par = for {
      i <- (0 until nSamples).par
      avgI1 = grid.avgLCauchyGreenI1(sampleZero, i)
    } yield (i, avgI1)
    // sort I1 by index order, and remove the index
    i1 = i1Par.seq.sortBy(_._1).map(_._2)
  }
  
}
