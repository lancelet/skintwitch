package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import skintwitch.MarkerGrid
import vtk.{ vtkActor, vtkGlyph3D, vtkPoints, vtkPolyData, vtkPolyDataMapper,
  vtkSphereSource }

class MarkerGridActor(grid: MarkerGrid) extends AnimatedActor {
  
  private var sample = 0
  private var points = new vtkPoints
  
  def getActors(): Seq[vtkActor] = {
    updatePoints()
    val polyData = new vtkPolyData {
      SetPoints(points)
    }
    val sphereSource = new vtkSphereSource {
      SetRadius(11.0)
    }
    val glyph3D = new vtkGlyph3D {
      SetInput(polyData)  // mesh for glyph on each vertex
      SetSource(sphereSource.GetOutput)
    }
    val polyDataMapper = new vtkPolyDataMapper {
      SetInputConnection(glyph3D.GetOutputPort)
    }
    val actor = new vtkActor {
      SetMapper(polyDataMapper)
      GetProperty.SetColor(0.5, 0.5, 0.5)
    }
    Seq(actor)
  }

  def setSample(index: Int) {
    sample = index
    updatePoints()
  }
  
  private def updatePoints() {
    points.Reset
    for {
      r <- 0 until grid.numRows
      c <- 0 until grid.numCols
      (x, y, z) = grid(r, c).co(sample)
    } points.InsertNextPoint(x, y, z)
    points.Modified
  }

}