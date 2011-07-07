package skintwitch.vtk

import scala.actors.Futures
import scala.collection.immutable._
import vtk.vtkActor2D
import vtk.vtkXYPlotActor
import vtk.vtkDoubleArray
import mocaputils.Marker
import skintwitch.MarkerGrid
import mocaputils.VirtualMarker
import vtk.vtkFieldData
import vtk.vtkDataObject
import javax.swing.SwingUtilities

class DistancePlotActor(
    staticMarkers: Seq[Marker], markers: Seq[Marker], grid: MarkerGrid,
    loadCallback: () => Unit, threshold: Double = 25.0 
) extends Animated2DActor {

  private var sample: Int = 0
  
  private val plotActor = new vtkXYPlotActor {
    SetYRange(-50, 50)
    ShowReferenceXLineOn
    SetWidth(0.3)
    SetHeight(0.2)
    GetAxisTitleTextProperty.ItalicOff
    GetAxisLabelTextProperty.ItalicOff
    SetXTitle("")
    SetYTitle("")
    SetXLabelFormat("")
    SetYLabelFormat("")
    SetPosition(0.01, 0.05)
    GetProperty.SetLineWidth(1.5)
    ShowReferenceYLineOn
    VisibilityOff
  }
 
  
  // distance at each sample index
  private def getDistances() = {
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
  private var distances: Seq[(Double, Boolean)] = Seq.empty[(Double, Boolean)]
  private val distFuture = Futures.future {
    distances = getDistances
    SwingUtilities.invokeLater(new Runnable {
      def run() = setDistancesInActor()
    })
  }
    
  def getActors(): Seq[vtkActor2D] = Seq(plotActor)
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def setDistancesInActor() {
    assert(SwingUtilities.isEventDispatchThread)
    val distanceArray = new vtkDoubleArray {
      SetJavaArray(distances.unzip._1.toArray)
    }
    val fieldData = new vtkFieldData {
      AddArray(distanceArray)
    }
    val dataObject = new vtkDataObject {
      SetFieldData(fieldData)
    }
    plotActor.AddDataObjectInput(dataObject)
    plotActor.SetReferenceYValue(
        distances.filter(_._2).map(_._1).min + threshold)
    plotActor.VisibilityOn
    update()
    loadCallback()
  }
  
  private def update() {
    plotActor.SetReferenceXValue(sample)
  }
  
}
