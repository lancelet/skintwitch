package skintwitch.rman

import scala.collection.immutable._
import ri._
import skintwitch.MarkerGrid
import simplex3d.math.double._
import simplex3d.math.double.functions._
import vtk.vtkCamera
import scalala.tensor.Matrix
import scalala.tensor.dense.DenseMatrix

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
  
  def evalPatch(u: Double, v: Double, 
    basis: Matrix[Double], P: Matrix[Double]): (Double, Double, Double) = {
    val U = DenseMatrix(Array(u*u*u, u*u, u, 1))
    val V = DenseMatrix(Array(v*v*v, v*v, v, 1))
    val r = (U * basis * P * basis.t * V.t).data
    (r(0), r(1), r(2))
  }

  def renderAnim(grid: MarkerGrid, camera: vtkCamera) {
    val nFrames = grid(0, 0).co.length
    for (frame <- 0 until nFrames by 4) {
      renderFrame(frame, grid, camera)
    }
  }
  
  def renderFrame(frame: Int, grid: MarkerGrid, camera: vtkCamera) {
    val riFunctions = new Ri()
    import riFunctions._
    Begin("aqsis") {
      
      Option("limits", "bucketsize", Seq(64, 64))
      Format(1280, 720, 1)
      PixelSamples(4, 4)
      PixelFilter(MitchellFilter, 1.11, 1.11)

      exportCamera(getContext(), camera)
      
      FrameBlock(1) {
        Display("render/%05d.tif" format frame, DisplayFile, DisplayRGB)
        Display("+%05d.tif" format frame, DisplayFrameBuffer, DisplayRGB)
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
          
          val p = (for {
            r <- Seq(Seq(0), 0 until grid.numRows, Seq(grid.numRows-1)).flatten
            c <- Seq(Seq(0), 0 until grid.numCols, Seq(grid.numCols-1)).flatten
            (x, y, z) = grid(r, c).co(frame)
          } yield {
            Seq(x, y, z)
          }).flatten
          AttributeBlock {
            ReverseOrientation
            Basis(CatmullRomBasis, 1, CatmullRomBasis, 1)
            PatchMesh(Bicubic, 
              grid.numCols+2, NonPeriodic, grid.numRows+2, NonPeriodic,
              "P", p)
          }
          
        }
        
      } // FrameBlock
      
    } // Begin
  }
  
}