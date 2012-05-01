package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import skintwitch.MarkerGrid
import vtk.{ vtkActor, vtkArrowSource, vtkDataArray, vtkDoubleArray, vtkPoints, 
  vtkPolyData, vtkPolyDataMapper, vtkTensorGlyph }
import scala.collection.mutable.ArrayBuilder

class BiotRateActor(grid: MarkerGrid, scale: Double = 100.0) 
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
    val tensorArrayBuilder = new ArrayBuilder.ofDouble
    tensorArrayBuilder.sizeHint(9 * grid.numRows * grid.numCols)
    val bg = grid.biotRate(sample)
    for {
      r <- 0 until grid.numRows
      c <- 0 until grid.numCols
      co = grid(r, c).co(sample)
      m = bg(r, c)
    } {
      points.InsertNextPoint(co.x, co.y, co.z)
      tensorArrayBuilder += (
        m.e11, m.e12, m.e13,
        m.e21, m.e22, m.e23,
        m.e31, m.e32, m.e33)
    }
    tensors.SetJavaArray(tensorArrayBuilder.result)
    points.Modified
    tensors.Modified
  }
  
}