package skintwitch.vtk

import scala.collection.immutable._
import mocaputils.Marker
import skintwitch.MarkerGrid
import vtk.{ vtkActor, vtkArrowSource, vtkDataArray, vtkDoubleArray, vtkPoints, 
  vtkPolyData, vtkPolyDataMapper, vtkTensorGlyph }
import scala.collection.mutable.ArrayBuilder

class BiotActor(grid: MarkerGrid, scale: Double = 100.0) 
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
      SetScaleFactor(100.0)
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
    val bg = grid.biot(0, sample)
    for {
      r <- 0 until grid.numRows
      c <- 0 until grid.numCols
      co = grid(r, c).co(sample)
      m = bg(r, c)
    } {
      points.InsertNextPoint(co.x, co.y, co.z)
      tensorArrayBuilder += (
        scale * m.e11, scale * m.e12, scale * m.e13,
        scale * m.e21, scale * m.e22, scale * m.e23,
        scale * m.e31, scale * m.e32, scale * m.e33)
    }
    tensors.SetJavaArray(tensorArrayBuilder.result)
    points.Modified
    tensors.Modified
  }
  
}