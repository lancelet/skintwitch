package skintwitch.vtk

import scala.collection.immutable._
import vtk.vtkActor2D

trait Animated2DActor {
  def getActors(): Seq[vtkActor2D]
  def setSample(index: Int)
}
