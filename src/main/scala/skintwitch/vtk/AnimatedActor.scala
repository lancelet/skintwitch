package skintwitch.vtk

import scala.collection.immutable._
import vtk.vtkActor

trait AnimatedActor {
  def getActors(): Seq[vtkActor]
  def setSample(index: Int)
}