package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import mocaputils.VirtualMarker
import skintwitch.MarkerGrid
import vtk.vtkActor
import vtk.vtkPoints
import vtk.vtkPolyData
import vtk.vtkCellArray
import vtk.vtkLine
import vtk.vtkPolyDataMapper

class PointerActor(
  staticMarkers: Seq[Marker], markers: Seq[Marker], grid: MarkerGrid
)
extends AnimatedActor {

  private var sample = 0
  private var points = new vtkPoints
  private var distPoints = new vtkPoints
  private val polyData = new vtkPolyData {
    SetPoints(points)
    val cells = new vtkCellArray {
      val lines = Seq((0, 1), (0, 2), (0, 3), (0, 4))  
      for (l <- lines) {
        val line = new vtkLine {
          GetPointIds.SetNumberOfIds(2)
          GetPointIds.InsertNextId(l._1)
          GetPointIds.InsertNextId(l._2)
        }
        InsertNextCell(line)
      }
    }
    SetLines(cells)
  }
  private val distPolyData = new vtkPolyData {
    SetPoints(distPoints)
    val cells = new vtkCellArray {
      InsertNextCell(new vtkLine {
        GetPointIds.SetNumberOfIds(2)
        GetPointIds.InsertNextId(0)
        GetPointIds.InsertNextId(1)
      })
    }
    SetLines(cells)
  }
  private val distPolyDataMapper = new vtkPolyDataMapper {
    SetInput(distPolyData)
  }
  private val distActor = new vtkActor {
    SetMapper(distPolyDataMapper)
    GetProperty.SetLineStipplePattern(Integer.parseInt("1100110011001100", 2))
    GetProperty.SetLineWidth(2.0)
  }
  
  // construct markers
  private def getMarker(name: String) = markers.find(_.name == name).get 
  private val mLong = getMarker("long")
  private val mMiddle = getMarker("middle")
  private val mShort = getMarker("short")
  private val mMed = getMarker("med")
  private val mTip: Marker = {
    def sm(name: String) = staticMarkers.find(_.name == name).get.co(0)
    val pLong = sm("long")
    val pMiddle = sm("middle")
    val pShort = sm("short")
    val pMed = sm("med")
    val pTip = sm("T6")
    new VirtualMarker("tip", pTip, Seq(
        (pLong, mLong), (pMiddle, mMiddle), (pShort, mShort), (pMed, mMed)))
  }
  private val pointerMarkers = Seq(mMiddle, mLong, mMed, mShort, mTip)
  
  def getActors(): Seq[vtkActor] = {
    update()
    val polyDataMapper = new vtkPolyDataMapper {
      SetInput(polyData)
    }
    val actor = new vtkActor {
      SetMapper(polyDataMapper)
      GetProperty.SetColor(0.5, 1.0, 0.5)
      GetProperty.SetLineWidth(4.0)
    }
    Seq(actor, distActor)
  }
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def update() {
    // update the points for the pointer
    points.Reset
    for {
      m <- pointerMarkers
      (x, y, z) = m.co(sample)
    } {
      points.InsertNextPoint(x, y, z)
    }
    points.Modified
    
    // update the points for the distance actor
    def ptIsInMesh(st: (Double, Double)): Boolean = {
      val minm = 1.0e-2
      val maxm = 1.0 - minm
      (st._1 > minm) && (st._1 < maxm) && (st._2 > minm) && (st._2 < maxm)
    }
    distPoints.Reset
    val triMesh = grid.diceToTrimesh(sample)
    val (dist, contactPt, st) = triMesh.signedDistanceTo(mTip.co(sample))
    val (xtip, ytip, ztip) = mTip.co(sample)
    val (xcon, ycon, zcon) = contactPt
    distPoints.InsertNextPoint(xtip, ytip, ztip)
    distPoints.InsertNextPoint(xcon, ycon, zcon)
    if (ptIsInMesh(st)) {
      if (dist < 0) {
        distActor.GetProperty.SetColor(1.0, 0.5, 0.5)
      } else {
        distActor.GetProperty.SetColor(1.0, 1.0, 1.0)
      }
    } else {
      distActor.GetProperty.SetColor(0.2, 0.2, 0.2)
    }
    distPoints.Modified
  }
  
}
