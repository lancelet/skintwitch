package skintwitch.rman

import vtk.vtkCamera

/** Camera parameters required to reconstruct a VTK view. */
case class CameraParams(viewAngle: Double, focalPt: V3, position: V3, 
                        rollAngle: Double)

object CameraParams {
  /** Obtain camera parameters from a VTK camera. */
  def apply(camera: vtkCamera): CameraParams = new CameraParams(
    camera.GetViewAngle, V3(camera.GetFocalPoint), V3(camera.GetPosition),
    camera.GetRoll)
}