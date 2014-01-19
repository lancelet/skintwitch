package skintwitch.rman

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable._
import scala.collection.parallel.immutable.ParSeq
import scala.util.Random
import ri._
import skintwitch.{ Grid, MarkerGrid, VGrid }
import simplex3d.math.double._
import simplex3d.math.double.functions._
import vtk.vtkCamera
import breeze.linalg.normalize
import breeze.linalg.Matrix
import breeze.linalg.{ DenseMatrix, DenseVector }
import java.io.{ BufferedOutputStream, BufferedWriter, File, FileOutputStream, 
                 OutputStreamWriter, FilenameFilter }
import simplex3d.noise.ClassicalGradientNoise
import V3._

object RManRender {

  private def renderFileName(frame: Int) = "./render/%05d.tif" format frame
  private def fileExists(name: String) = (new File(name)).exists
  
  def renderAnim(grid: MarkerGrid, camera: vtkCamera, 
                 overwrite: Boolean = false) 
  {
    val nFrames = grid(0, 0).co.length
    for (frame <- (0 until nFrames by 4).par) {
      // only render if the overwrite flag is set, or if the file doesn't exist
      if (overwrite || !fileExists(renderFileName(frame))) {
        renderFrame(frame, grid, camera)
      }
      
      // clean-up RIB files post-render
      val ribFileNamesForFrame = (new File("./rib/")).list(
        new FilenameFilter {
          def accept(dir: File, name: String) = 
            name.startsWith("f%05d" format frame)
        }
      )
      for (fileName <- ribFileNamesForFrame) {
        println("Post-render; deleting RIB file \"%s\"" format fileName)
        (new File("./rib/" + fileName)).delete
      }
    }
  }
  
  def renderFrame(frame: Int, grid: MarkerGrid, camera: vtkCamera) {
    val riFunctions = new Ri()
    import riFunctions._
    Begin("aqsis") {
      
      Option("searchpath", "shader", "./shaders:&")
      Option("limits", "bucketsize", Seq(32, 32))
      //Format(1920, 1080, 1)
      Format(1280, 720, 1)
      PixelSamples(10, 10)
      PixelFilter(GaussianFilter, 1.4, 1.4)

      TransformBlock {
        Rotate(30, 0, 1, 0)
        Rotate(30, 1, 0, 0)
        LightSource("distantlight")
      }
      
      ProjectionUtils.exportCamera(getContext, CameraParams(camera))
      
      FrameBlock(1) {
        Display(renderFileName(frame), DisplayFile, DisplayRGB)
        Display("+%05d.tif" format frame, DisplayFrameBuffer, DisplayRGB)
        Sides(1)
        
        WorldBlock {
          
          // render spheres for the markers
          for {
            r <- 0 until grid.numRows
            c <- 0 until grid.numCols
            val co = grid(r, c).co(frame)
          } AttributeBlock {
            Surface("matte")
            Translate(co.x, co.y, co.z)
            Scale(10, 10, 10)
            Sphere(1, -1, 1, 360)
          }

          // expand grid, so that edges are linearly interpolated outward
          val v3grid = grid.map { m =>
            val co = m.co(frame)
            V3(co.x, co.y, co.z)
          }
          val egrid = v3grid.expandEdgesByOne
          
          // render bicubic patch mesh for the skin
          val p = egrid.rowMajor.map(v => Seq(v.e0, v.e1, v.e2)).flatten
          AttributeBlock {
            Surface("matte")
            ReverseOrientation
            Basis(CatmullRomBasis, 1, CatmullRomBasis, 1)
            PatchMesh(Bicubic, 
              egrid.numCols, NonPeriodic, egrid.numRows, NonPeriodic,
              "P", p)
          }
          
          // hair; here, we approximate the bicubic patch using a tesselated
          //  bilinear patch
          /*
          val bcpm = ApproxBicubicPatchMesh(
            BicubicPatchMesh.catmullRom, 1,
            BicubicPatchMesh.catmullRom, 1,
            grid.numCols+2, grid.numRows+2,
            p.toIndexedSeq,
            100, 100
          )
          renderHair(getContext, bcpm, frame)
          */
          
        }
        
      } // FrameBlock
      
    } // Begin
  }
  
  def renderHair(context: Context, bcpm: ApproxBicubicPatchMesh, frame: Int) {
    val riFunctions = new Ri()
    import riFunctions._
    
    Resume(context) {
      AttributeBlock {
        Basis(CatmullRomBasis, 1, CatmullRomBasis, 1)
        Color(Seq(0.243, 0.145, 0.153))
        Surface("hair_gritz", 
                "color rootcolor", Seq(0.1, 0.05, 0.05),
                "color tipcolor", Seq(0.243, 0.145, 0.153))
        def fileName(index: Int) = 
          "./rib/f%05dhair%05d.rib" format (frame, index)
        for ((hp, index) <- (hairPatches zipWithIndex).par) {
          hp.evalToFile(bcpm, fileName(index))
        }
        for ((hp, index) <- (hairPatches.zipWithIndex)) {
          DelayedReadArchive(fileName(index), hp.bounds(bcpm, 50.0))
        }
      }
    }
  }

  case class Hair(u: Double, v: Double, 
                  noisen: Double, noisev: Double,
                  nclump: Double,
                  length: Double = 10.0)
  case class HairPatch(minU: Double, maxU: Double,
                       minV: Double, maxV: Double,
                       noiseSeed: Int, nHairs: Int) 
  {
    val r = new Random(noiseSeed)
    val noise = new ClassicalGradientNoise(0)
    val hairs: ParSeq[Hair] = ({
      val urange = maxU - minU
      val vrange = maxV - minV
      for (i <- 0 until nHairs) yield {
        val u = minU + r.nextDouble * urange
        val v = minV + r.nextDouble * vrange
        val noisen = r.nextGaussian
        val noisev = if (r.nextDouble > 0.99) {
          r.nextGaussian * 10
        } else {
          r.nextGaussian
        }
        val length = if (r.nextDouble > 0.8) {
          12.0 + r.nextGaussian * 5.0 + noise(u * 40, v * 11) * 3.0
        } else {
          5.0 + r.nextGaussian * 2.0 + noise(u * 40, v * 11) * 2.0
        }
        val nclump = noise(u * 35, v * 10)
        Hair(u, v, noisen, noisev, nclump, length)
      }
    }).par
    def bounds(bcpm: ApproxBicubicPatchMesh, growBy: Double): BoundBox = {
      val corners = Seq(
        bcpm(minU, minV).p,
        bcpm(minU, maxV).p,
        bcpm(maxU, minV).p,
        bcpm(maxU, maxV).p
      )
      BBox(corners).growBy(growBy).rman
    }
    def evalToFile(bcpm: ApproxBicubicPatchMesh, fileName: String) = 
    {
      val hairp = (for {
        h <- hairs.par
        bcr = bcpm(h.u, h.v)
        p = bcr.p
        n = bcr.n * (1.0 + 0.1 * h.noisen + 0.35 * h.nclump)
        u = bcr.dpdu.normalized
        v = bcr.dpdv.normalized
        p0 = p - u * h.length
        p1 = p
        p2 = p + (u + n*0.2) * h.length + (v * 0.05) * h.noisev
        p3 = p + (u*2 + n*0.2) * h.length + (v * 0.1) * h.noisev
      } yield Seq(p0.e0, p0.e1, p0.e2,
                  p1.e0, p1.e1, p1.e2,
                  p2.e0, p2.e1, p2.e2,
                  p3.e0, p3.e1, p3.e2)
      ).seq.flatten
      
      val fs = new BufferedOutputStream(new FileOutputStream(fileName))
      val fw = new BufferedWriter(new OutputStreamWriter(fs))
      fw.write("Curves \"cubic\" \n")
      fw.write("[ "); for (i <- 0 until nHairs) fw.write("4 "); fw.write("]\n")
      fw.write("\"nonperiodic\"\n")
      fw.write("\"constantwidth\" [ 0.3 ]\n")
      fw.write("\"P\"\n")      
      fw.write("[ ")
      for (p <- hairp) fw.write("%.2f " format p)
      fw.write("]\n")
      fs.flush
      fw.flush
      fw.close
    }
  }
  val nHairs = 4000000
  //val nHairs = 500000
  val nPatches = 10
  val nHairsPerPatch = nHairs / (nPatches * nPatches)
  val hairPatches = {
    val r = new Random(100)
    (for (uPatch <- 0 until nPatches) yield {
      val minU = uPatch.toDouble / nPatches
      val maxU = (uPatch + 1).toDouble / nPatches
      for (vPatch <- 0 until nPatches) yield {
        val minV = vPatch.toDouble / nPatches
        val maxV = (vPatch + 1).toDouble / nPatches
        HairPatch(minU, maxU, minV, maxV, r.nextInt, nHairsPerPatch)
      }
    }).flatten
  }
      
}
