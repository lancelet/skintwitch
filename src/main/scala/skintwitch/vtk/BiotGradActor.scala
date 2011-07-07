package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import skintwitch.MarkerGrid
import vtk.{ vtkActor, vtkArrowSource, vtkDataArray, vtkDoubleArray, vtkPoints, 
  vtkPolyData, vtkPolyDataMapper, vtkSphereSource, vtkTensorGlyph }

class BiotGradActor(grid: MarkerGrid, scale: Double = 100.0) 
extends AnimatedActor {

  private var sample = 0
  private var points = new vtkPoints
  private var tensors = new vtkDoubleArray {
    SetNumberOfTuples(grid.numRows * grid.numCols)
    SetNumberOfComponents(9)
  }
  private var polyData = new vtkPolyData {
    SetPoints(points)
    GetPointData.SetTensors(tensors)
  }
  
  def getActors(): Seq[vtkActor] = {
    update()
    /*
    val sphereSource = new vtkSphereSource {
      SetRadius(20.0)
    }
    */
    val arrowSource = new vtkArrowSource {
      
    }
    val tensorGlyph = new vtkTensorGlyph {
      SetInput(polyData)
      SetSource(arrowSource.GetOutput)
      SetScaleFactor(scale)
      ScalingOn
      ColorGlyphsOn
      ThreeGlyphsOn
      SetColorModeToEigenvalues
      ExtractEigenvaluesOn
      SymmetricOn
    }
    val polyDataMapper = new vtkPolyDataMapper {
      SetInputConnection(tensorGlyph.GetOutputPort)
    }
    val actor = new vtkActor {
      SetMapper(polyDataMapper)
    }
    Seq(actor)
  }
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def update() {
    points.Reset
    tensors.Reset
    val bg = grid.biotGrad(sample)
    for {
      r <- 0 until grid.numRows
      c <- 0 until grid.numCols
      (x, y, z) = grid(r, c).co(sample)
      m = bg(r, c)
    } {
      points.InsertNextPoint(x, y, z)
      tensors.InsertNextTuple9(
        m(0, 0), m(0, 1), m(0, 2),
        m(1, 0), m(1, 1), m(1, 2),
        m(2, 0), m(2, 1), m(2, 2))
    }
    points.Modified
    tensors.Modified
  }
  
}