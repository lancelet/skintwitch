package skintwitch.vtk

import scala.actors.Futures
import scala.collection.immutable._
import vtk.vtkActor2D
import vtk.vtkXYPlotActor
import vtk.vtkDoubleArray
import mocaputils.Marker
import skintwitch.Vec2
import skintwitch.Vec3
import skintwitch.MarkerGrid
import mocaputils.VirtualMarker
import vtk.vtkFieldData
import vtk.vtkDataObject
import javax.swing.SwingUtilities
import vtk.vtkTextActor

class DistancePlotActor(
    staticMarkers: Seq[Marker], markers: Seq[Marker], grid: MarkerGrid,
    loadCallback: () => Unit, threshold: Double = 10.0 
) extends Animated2DActor {

  private var sample: Int = 0
  
  private val lblActor = new vtkTextActor {
    GetPositionCoordinate.SetCoordinateSystemToNormalizedViewport
    SetInput("Computing distance data...")
    SetPosition(0.01, 0.05)
    GetProperty.SetColor(0.4, 0.4, 0.4)
  }
  
  private val plotActor = new vtkXYPlotActor {
    SetYRange(-50, 50)
    ShowReferenceXLineOn
    SetWidth(0.3)
    SetHeight(0.15)
    GetAxisTitleTextProperty.ItalicOff
    GetAxisLabelTextProperty.ItalicOff
    SetXTitle("")
    SetYTitle("")
    SetXLabelFormat("")
    SetYLabelFormat("")
    SetPosition(0.01, 0.05)
    GetProperty.SetLineWidth(1.5)
    GetProperty.SetColor(0.6, 0.6, 0.6)
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
    
    def inGrid(st: Vec2): Boolean = { 
      val minm = 0.01
      val maxm = 1.0 - minm
      (st.x > minm) && (st.x < maxm) && (st.y > minm) && (st.y < maxm)
    }
    
    // find distance to the mesh at each time index
    for {
      i <- 0 until markers(0).co.length
      triMesh = grid.diceToTrimesh(i)
      (dist, xPoint, st) = triMesh.signedDistanceTo(Vec3(mTip.co(i)))
    } yield (dist, inGrid(st))
  }
  private var distances: Seq[(Double, Boolean)] = Seq.empty[(Double, Boolean)]
  private val distFuture = Futures.future {
    distances = getDistances
    SwingUtilities.invokeLater(new Runnable {
      def run() = setDistancesInActor()
    })
  }
    
  def getActors(): Seq[vtkActor2D] = Seq(plotActor, lblActor)
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def setDistancesInActor() {
    assert(SwingUtilities.isEventDispatchThread)
    if (distances.exists(_._2)) {
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
      lblActor.SetInput("Distance")
      lblActor.SetPosition(0.01 + 0.3 - 0.15,
                           0.05 + 0.15 - 0.05)
      loadCallback()
    }
  }
  
  private def update() {
    plotActor.SetReferenceXValue(sample)
  }
  
}
