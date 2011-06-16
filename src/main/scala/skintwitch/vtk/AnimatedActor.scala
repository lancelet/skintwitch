package skintwitch.vtk

import vtk.vtkActor

trait AnimatedActor {
  def getActor(): vtkActor
  def setSample(index: Int)
}