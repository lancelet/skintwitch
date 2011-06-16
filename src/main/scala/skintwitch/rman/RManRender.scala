package skintwitch.rman

import ri._
import skintwitch.MarkerGrid
import simplex3d.math.double._
import simplex3d.math.double.functions._
import vtk.vtkCamera

object RManRender {

  def exportCamera(context: Context, camera: vtkCamera) = {
    val riFunctions = new Ri()
    import riFunctions._
    
    val angle = camera.GetViewAngle
    
    Resume(context) {
      Projection(PerspectiveProjection, "fov", angle)
      placeCamera(getContext(), camera)
      Orientation(RightHanded)
    }
  }

  def placeCamera(context: Context, camera: vtkCamera) {
    val riFunctions = new Ri()
    import riFunctions._

    val (fx, fy, fz) = { val f = camera.GetFocalPoint; (f(0), f(1), f(2)) }
    val (px, py, pz) = { val p = camera.GetPosition; (p(0), p(1), p(2)) }
    val (dxu, dyu, dzu) = (fx - px, fy - py, fz - pz)
    val duMag = math.sqrt(dxu * dxu + dyu * dyu + dzu * dzu)
    val (dx, dy, dz) = (dxu / duMag, dyu / duMag, dzu / duMag)
    val roll = camera.GetRoll
    
    Resume(context) {
      Identity()
      Transform(Mat4(-1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1))
      Rotate(-roll, 0, 0, 1)
      aimZ(getContext(), dx, dy, dz)
      Translate(-px, -py, -pz)
    }
  }
  
  def aimZ(context: Context, dx: Double, dy: Double, dz: Double) {
    val riFunctions = new Ri()
    import riFunctions._

    val xzlen = math.sqrt(dx * dx + dz * dz)
    val yrot = if (xzlen == 0) {
      if (dy < 0) 180.0 else 0.0
    } else {
      180.0 * math.acos(dz / xzlen) / math.Pi
    }
      
    val yzlen = math.sqrt(dy * dy + xzlen * xzlen)
    val xrot = 180.0 * math.acos(xzlen / yzlen) / math.Pi
      
    Resume(context) {
      Rotate(xrot, if (dy > 0) 1 else -1, 0, 0)
      Rotate(yrot, 0, if (dx > 0) -1 else 1, 0)
    }
  }
  
  def renderFrame(frame: Int, grid: MarkerGrid, camera: vtkCamera) {
    val riFunctions = new Ri()
    import riFunctions._
    Begin("aqsis") {
      
      Option("limits", "bucketsize", Seq(64, 64))
      Format(1280, 720, 1)
      PixelSamples(2, 2)
      PixelFilter(GaussianFilter, 2, 2)

      exportCamera(getContext(), camera)
      
      FrameBlock(1) {
        Display("+test.tif", DisplayFrameBuffer, DisplayRGB)
        Sides(1)
        
        WorldBlock {
          
          for {
            r <- 0 until grid.numRows
            c <- 0 until grid.numCols
            (x, y, z) = grid(r, c).co(frame)
          } TransformBlock {
            Translate(x, y, z)
            Scale(10, 10, 10)
            Sphere(1, -1, 1, 360)
          }
          
        }
        
      } // FrameBlock
      
    } // Begin
  }
  
}