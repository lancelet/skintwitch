package skintwitch.rman

import ri.{ Context, PerspectiveProjection, Ri, RightHanded }
import simplex3d.math.double.Mat4
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
    
    Resume(context) {
      Projection(PerspectiveProjection, "fov", camParams.viewAngle)
      Identity
      ConcatTransform(cameraToWorld(camParams))
      Orientation(RightHanded)
    }    
  }
  
  /** Obtains the Camera->World transformation matrix. */
  def cameraToWorld(camParams: CameraParams): M4 = {
    val direction = (camParams.focalPt - camParams.position).normalized
    // Note: matrix multiplications occur in the reverse order of normal
    //       RenderMan transformations
    M4.translate(-camParams.position.e0,
                 -camParams.position.e1,
                 -camParams.position.e2) *
      aimZMatrix(direction.e0, direction.e1, direction.e2) *
      M4.rotate(-camParams.rollAngle, 0, 0, 1) *
      M4.scale(-1, 1, 1)
  }
  
  /** Obtains a matrix which aligns the z-axis of the coordinate system with
   *  a given axis. */
  def aimZMatrix(dx: Double, dy: Double, dz: Double): M4 = {
    val xzlen = sqrt(dx * dx + dz * dz)
    val yrot = if (xzlen == 0) { if (dy < 0) 180.0 else 0.0 } 
               else { 180.0 * acos(dz / xzlen) / Pi }
      
    val yzlen = sqrt(dy * dy + xzlen * xzlen)
    val xrot = 180.0 * acos(xzlen / yzlen) / Pi

    val r1 = M4.rotate(xrot, if (dy > 0) 1 else -1, 0, 0)
    val r2 = M4.rotate(yrot, 0, if (dx > 0) -1 else 1, 0)
    r2 * r1
  }
  
  /** Camera look-at transform.
   *  
   *  @param context RenderMan context to use
   *  @param camPos position of the camera
   *  @param focalPt viewing position
   *  @param up up vector */
  def cameraLookAt(context: Context, camPos: V3, focalPt: V3, up: V3) {
    val riFunctions = new Ri()
    import riFunctions._
    
    val z = (focalPt - camPos).normalized
    val x = (up.normalized x z) * -1
    val y = z x x
    val m = Mat4(x.e0, y.e0, z.e0, 0,
                 x.e1, y.e1, z.e1, 0,
                 x.e2, y.e2, z.e2, 0,
                    0,    0,    0, 1)
                    
    Resume(context) {
      ConcatTransform(m) // rotation only
      Translate(-camPos.e0, -camPos.e1, -camPos.e2)
    }
  }
  
  def objectLookAt(objPos: V3, focalPt: V3, up: V3): M4 = {
    val zz = (focalPt - objPos).normalized
    val xx = (up x zz).normalized
    val yy = zz x xx
    
    /*
    M4(xx.e0, yy.e0, zz.e0, 0,
       xx.e1, yy.e1, zz.e1, 0,
       xx.e2, yy.e2, zz.e2, 0,
           0,     0,     0, 1) * */
    M4(xx.e0, xx.e1, xx.e2, 0,
       yy.e0, yy.e1, yy.e2, 0,
       zz.e0, zz.e1, zz.e2, 0,
           0,     0,     0, 1) *
        M4.translate(objPos.e0, objPos.e1, objPos.e2)

  }
  
}