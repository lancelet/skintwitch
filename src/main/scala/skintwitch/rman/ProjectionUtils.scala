package skintwitch.rman

import ri.{ Context, PerspectiveProjection, Ri, RightHanded }
import math.{ acos, Pi, sqrt }
import vtk.vtkCamera

object ProjectionUtils {

  /** Exports VTK camera (via stored parameters) to a RIB stream.
   *  
   *  @param context RenderMan context to use.
   *  @param camera camera parameters to use for viewing parameters */  
  def exportCamera(context: Context, camParams: CameraParams) {
    val riFunctions = new Ri()
    import riFunctions._
    
    val direction = (camParams.focalPt - camParams.position).normalized
    
    Resume(context) {
      Projection(PerspectiveProjection, "fov", camParams.viewAngle)
      Identity
      Scale(-1, 1, 1)
      Rotate(-camParams.rollAngle, 0, 0, 1)
      aimZ(getContext, direction.e0, direction.e1, direction.e2)
      Translate(-camParams.position.e0, -camParams.position.e1, 
                -camParams.position.e2)
      Orientation(RightHanded)
    }    
  }
  
  /** Rotates the coordinate system so that its z-axis points along a
   *  specified vector.
   *  
   *  @param context RenderMan context to use
   *  @param dx x component of the vector
   *  @param dy y component of the vector
   *  @param dz z component of the vector */
  def aimZ(context: Context, dx: Double, dy: Double, dz: Double) {
    val riFunctions = new Ri()
    import riFunctions._

    val xzlen = sqrt(dx * dx + dz * dz)
    val yrot = if (xzlen == 0) { if (dy < 0) 180.0 else 0.0 } 
               else { 180.0 * acos(dz / xzlen) / Pi }
      
    val yzlen = sqrt(dy * dy + xzlen * xzlen)
    val xrot = 180.0 * acos(xzlen / yzlen) / Pi
      
    Resume(context) {
      Rotate(xrot, if (dy > 0) 1 else -1, 0, 0)
      Rotate(yrot, 0, if (dx > 0) -1 else 1, 0)
    }
  }
  
}