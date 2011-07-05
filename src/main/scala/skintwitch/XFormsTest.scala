package skintwitch

import scala.collection.immutable._
import ri.DisplayFrameBuffer
import ri.DisplayRGB
import ri.GaussianFilter
import ri.PerspectiveProjection
import ri.Ri
import skintwitch.rman.M4
import ri.Context
import skintwitch.rman.ProjectionUtils
import skintwitch.rman.V3

class XFormsTest { }

object XFormsTest {
  def main(args: Array[String]) {
    val riFunctions = new Ri()
    import riFunctions._

    Begin("aqsis") {
      FrameBlock(1) {

        Option("limits", "bucketsize", Seq(32, 32))
        Format(800, 600, 1)
        PixelSamples(2, 2)
        PixelFilter(GaussianFilter, 1.2, 1.2)
        Display("+test", DisplayFrameBuffer, DisplayRGB)
        
        Projection(PerspectiveProjection, "fov", 30)
        Translate(0,0,30)
        
        WorldBlock {
          Sides(1)
          AttributeBlock {
            Scale(0.1, 0.1, 0.1)
            triad(getContext)
          }
          AttributeBlock {
            val m = ProjectionUtils.objectLookAt(V3(7.5, 0, 0), V3(0,0,0), V3(0,0,1))  // pos, focalPt, up
            ConcatTransform(m)
            triad(getContext)
          }
        } // WorldBlock

      } // FrameBlock
    } // Begin
  }
  
  def triad(context: Context) {
    val riFunctions = new Ri()
    import riFunctions._
    
    Resume(context) {
      AttributeBlock {
        AttributeBlock {  // x-arrow
          Color(Seq(1,0,0))
          Rotate(90, 0,1,0)
          arrowz(getContext)
        }
        AttributeBlock {  // y-arrow
          Color(Seq(0,1,0))
          Rotate(-90, 1,0,0)
          arrowz(getContext)
        }
        AttributeBlock {  // z-arrow
          Color(Seq(0,0,1))
          arrowz(getContext)
        }
      }
    }
  }
  
  // arrow of height 7.5
  def arrowz(context: Context) {
    val riFunctions = new Ri()
    import riFunctions._
    
    Resume(context) {
      Cylinder(0.5, 0, 5, 360)
      AttributeBlock {
        ReverseOrientation
        Disk(0, 0.5, 360)
      }
      Hyperboloid(Seq(0.5, 0, 5), Seq(1.0, 0, 5), 360)
      TransformBlock {
        Translate(0, 0, 5)
        Cone(2.5, 1, 360)
      }
    }
  }
}