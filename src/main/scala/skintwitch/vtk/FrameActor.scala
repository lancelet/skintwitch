package skintwitch.vtk

import scala.collection.immutable._
import vtk.vtkActor2D
import vtk.vtkTextActor

/** Displays frame number in the VTK plot. */
class FrameActor extends Animated2DActor {
  
  private var sample: Int = 0
  private val txtActor = new vtkTextActor {
    SetPosition(10, 10)
    GetProperty.SetColor(1,1,1)
  }
  
  def getActors(): Seq[vtkActor2D] = {
    update()
    Seq(txtActor)
  }
  
  def setSample(index: Int) {
    sample = index
    update()
  }
  
  private def update() {
    txtActor.SetInput("Frame: %d" format sample)
  }
  
}