package skintwitch

import java.io.{BufferedReader, File, FilenameFilter, InputStreamReader}
import scala.collection.immutable._
import mocaputils.{GapFiller, TRCData, TRCReader}
import skintwitch.rman.{ProjectionUtils, RenderOptions, V3, R4}
import ri.{Bicubic, CatmullRomBasis, DisplayFile, DisplayFrameBuffer, 
           DisplayRGB, DisplayRGBA, GaussianFilter, NonPeriodic, 
           PerspectiveProjection, Ri}
import ri.RightHanded
import ri.OrthographicProjection
import ri.DisplayShadow
import ri.DisplayZ
import ri.BoxFilter
import ri.MitchellFilter

object AqsisRender {
  /** Main program entry point. */
  def main(args: Array[String]) {
    val renderOptionsFileName = args(0)
    render(renderOptionsFileName)
  }
  
  /** Renders a sequence.
   *  @param renderOptionsFileName name of the render options file (JSON
   *         format */
  def render(renderOptionsFileName: String) {
    render(RenderOptions.loadRenderOptions(renderOptionsFileName))
  }
  
  /** Renders a sequence.
   *  @param renderOptions rendering options */
  def render(renderOptions: RenderOptions) {
    val trcData = TRCReader.read(renderOptions.trialFileName).fold(
      failure => return, success => success)
    createOutputDirectories()
    compileShaders()
    val frames = 0 until trcData.numFrames by renderOptions.renderNthSample
    //val frameSet = frames zip frames.tail
    //val frameSet = Seq((156, 172))
    val frameSet = Seq((0, 4))
    for ((curFrame, nextFrame) <- frameSet) {
      renderFrame(renderOptions, curFrame, nextFrame, Some(trcData))
    }
  }

  /** Returns the file name for a given rendered frame. */
  private def renderFileName(frame: Int) = "./render/%05d.tif" format frame
  /** Returns the world file name for a given rendered frame. */
  private def worldFileName(frame: Int) = "./rib/f%05d-world.rib" format frame
  /** Returns the z file name for a given rendered frame. */
  private def zFileName(frame: Int) = "./maps/%05d-shadow.z" format frame
  /** Returns the shadow file name for a given rendered frame. */
  private def shdFileName(frame: Int) = "./maps/%05d-shadow.shd" format frame
  /** Returns the point cloud name for a given rendered frame. */
  private def ptFileName(frame: Int) = "./maps/%05d-ptcloud.ptc" format frame
  /** Returns the point cloud world file name for a given frame. */
  private def ptWorldFileName(frame: Int) = 
    "./rib/f%05d-ptworld.rib" format frame
  
  /** Renders an individual frame.
   *  @param renderOptions rendering options
   *  @param curFrame the current frame
   *  @param nextFrame the next frame (used for motion blur) */
  private def renderFrame(renderOptions: RenderOptions, curFrame: Int, 
                          nextFrame: Int, trcDataOpt: Option[TRCData]) 
  {
    val riFunctions = new Ri()
    import riFunctions._
    
    val trcData = trcDataOpt.getOrElse {
      TRCReader.read(renderOptions.trialFileName).fold(
        failure => return, success => success)}
    val markers = trcData.markers.map(GapFiller.fillGapsLerp(_).get) 
    val grid = MarkerGrid.fromCRMarkers(markers)    
    
    // export the world geometry for the beauty + shadow passes
    exportWorld(worldFileName(curFrame), renderOptions, curFrame, nextFrame,
      trcData)
    // export world geometry for the point cloud pass
    exportWorld(ptWorldFileName(curFrame), renderOptions, curFrame, nextFrame,
      trcData, true)
    
    // center of the grid
    val gridCenter = {
      val p = grid.avgPosition
      V3(p._1, p._2, p._3)
    }
    // camera parameters
    val camParams = renderOptions.camParams
    // light position (relative to camera, in camera space)
    val relLightPosition = V3(3000, 3000, -3000)
    val worldLightPosition = R4(relLightPosition + camParams.position, 1) * 
                             ProjectionUtils.cameraToWorld(camParams).inv
    val focalDistance = (camParams.position - gridCenter).length
      
    // shadow map pass
    Begin("aqsis" /*"smap.rib"*/) {
      FrameBlock(1) {
        Option("searchpath", "shader", "./shaders:&")
        Option("limits", "bucketsize", Seq(64, 64))
        Display(zFileName(curFrame), DisplayShadow, DisplayZ)
        Display("+shadow", DisplayFrameBuffer, DisplayRGB)
        PixelSamples(1, 1)
        PixelFilter(BoxFilter, 1, 1)
        val sMapSize = 2 * math.max(renderOptions.xRes, renderOptions.yRes)
        Format(sMapSize, sMapSize, 1)
        ShadingRate(8.0)
        Hider("hidden", "depthfilter", "midpoint", "jitter", 0)
        LightSource("distantlight")
        Clipping(100, 5000)
        val s = 500
        ScreenWindow(-s, s, -s, s)
        Projection(OrthographicProjection)
        Scale(-1,1,1)
        val m = ProjectionUtils.objectLookAt(
            worldLightPosition, gridCenter, V3(0,0,1))
        ConcatTransform(m.inv)
        WorldBlock {
          Sides(1)
          ReadArchive(worldFileName(curFrame))
        } // WorldBlock
      } // Frameblock
      MakeShadow(zFileName(curFrame), shdFileName(curFrame))
    } // Begin
    
    // point cloud pass
    Begin("aqsis" /*"ptcloud.rib"*/) {
      FrameBlock(1) {
        Option("searchpath", "shader", "./shaders:&")
        Option("limits", "bucketsize", Seq(32, 32))
        Format(renderOptions.xRes, renderOptions.yRes, 1)
        PixelSamples(1, 1)
        PixelFilter(BoxFilter, 1, 1)
        ShadingRate(1.0)
        ProjectionUtils.exportCamera(getContext, renderOptions.camParams)
        Display("+ptcloud", DisplayFrameBuffer, DisplayRGB)
        Hider("hidden", "jitter", 0)
        Clipping(100, 5000)
        Attribute("cull", "float hidden", 0)
        Attribute("cull", "float backfacing", 0)
        Attribute("dice", "float rasterorient", 0)
        WorldBlock {
          // place a light source relative to the camera
          TransformBlock {
            val m = ProjectionUtils.objectLookAt( 
              worldLightPosition, gridCenter, V3(0,0,1))
            ConcatTransform(m)
            LightSource("sdistant",
                        "float intensity", 0.6,
                        "string shadowname", shdFileName(curFrame),
                        "float bias", 3.0)
          } // TransformBlock

          ReadArchive(ptWorldFileName(curFrame))          
        }
      } // FrameBlock
    } // Begin
    
    // beauty pass
    Begin("aqsis" /*"beauty.rib"*/) {
      FrameBlock(1) {
        // basic rendering options and frame setup
        Option("searchpath", "shader", "./shaders:&")
        Option("limits", "bucketsize", Seq(32, 32))
        Format(renderOptions.xRes, renderOptions.yRes, 1)
        PixelSamples(renderOptions.xSamples, renderOptions.ySamples)
        PixelFilter(MitchellFilter, 3.0, 3.0)
        Shutter(0.0, 1.0)
        Clipping(100, 5000)
        DepthOfField(4.0, 50.0, focalDistance)  // f4 50mm lens
        
        // export camera
        ProjectionUtils.exportCamera(getContext, renderOptions.camParams)
        
        Display(renderFileName(curFrame), DisplayFile, DisplayRGB)
        Display("+%05d.tif" format curFrame, DisplayFrameBuffer, DisplayRGB)
        WorldBlock {
          // place a light source relative to the camera
          TransformBlock {
            val m = ProjectionUtils.objectLookAt( 
              worldLightPosition, gridCenter, V3(0,0,1))
            ConcatTransform(m)
            LightSource("sdistant",
                        "float intensity", 0.6,
                        "string shadowname", shdFileName(curFrame),
                        "float bias", 3.0)
          } // TransformBlock
          Sides(1)
          ReadArchive(worldFileName(curFrame))
        } // WorldBlock
      } // FrameBlock
    } // Begin
    
    /*
    deleteFilesPostRender(Seq(
      worldFileName(curFrame),
      zFileName(curFrame),
      shdFileName(curFrame),
      ptWorldFileName(curFrame),
      ptFileName(curFrame)))
      */
  }
  
  private def deleteFilesPostRender(files: Seq[String]) {
    files.map(new File(_).delete)
  }
  
  private def exportWorld(worldFileName: String,
                          renderOptions: RenderOptions, curFrame: Int, 
                          nextFrame: Int, trcData: TRCData,
                          pointCloud: Boolean = false)
  {
    val riFunctions = new Ri()
    import riFunctions._

    val markers = trcData.markers.map(GapFiller.fillGapsLerp(_).get) 
    val grid = MarkerGrid.fromCRMarkers(markers)    
    
    Begin(worldFileName) {      
      
      // render spheres for the markers
      if (renderOptions.renderMarkers) {
        for {
          r <- 0 until grid.numRows
          c <- 0 until grid.numCols
          (x0, y0, z0) = grid(r, c).co(curFrame)
          (x1, y1, z1) = grid(r, c).co(nextFrame)
        } AttributeBlock {
          Surface("matteocclusion", 
                  "float Ko", 0.4,
                  "string ptCloudName", ptFileName(curFrame),
                  "float baking", if (pointCloud) 1 else 0)
          MotionBlock(Seq(0, 1)) {
            Translate(x0, y0, z0)
            Translate(x1, y1, z1)
          } // MotionBlock
          Scale(10, 10, 10)
          Sphere(1, -1, 1, 360)
        } // AttributeBlock
      }
      
      // render bicubic patch mesh for the skin
      if (renderOptions.renderSkinGrid) {
        val egrid0 = (grid.map { marker =>
          val co = marker.co(curFrame)
          V3(co._1, co._2, co._3)
        }).expandEdgesByOne
        val egrid1 = (grid.map { marker =>
          val co = marker.co(nextFrame)
          V3(co._1, co._2, co._3)
        }).expandEdgesByOne
        val p0 = egrid0.rowMajor.map(v => Seq(v.e0, v.e1, v.e2)).flatten
        val p1 = egrid1.rowMajor.map(v => Seq(v.e0, v.e1, v.e2)).flatten
        AttributeBlock {
          Surface("skingrid",
                  "float Ko", 0.4,
                  "string ptCloudName", ptFileName(curFrame),
                  "float baking", if (pointCloud) 1 else 0)
          //Surface("matte")
          ReverseOrientation
          Basis(CatmullRomBasis, 1, CatmullRomBasis, 1)
          MotionBlock(Seq(0, 1)) {
            PatchMesh(Bicubic,
              egrid0.numCols, NonPeriodic, egrid0.numRows, NonPeriodic,
              "P", p0)
            PatchMesh(Bicubic,
              egrid1.numCols, NonPeriodic, egrid1.numRows, NonPeriodic,
              "P", p1)
          } // MotionBlock
        } // AttributeBlock
      }
      
    } // Begin
}
  
  /** Creates any output directories required for the rendering. */
  private def createOutputDirectories() {
    val outputDirs = Seq("./render", "./rib", "./maps")
    for (dir <- outputDirs) {
      val file = new File(dir)
      if (!file.exists) {
        file.mkdirs
      } else {
        if (!file.isDirectory) {
          println("File \"%s\" is supposed to be a directory, but isn't!"
            format file.getCanonicalPath)
        }
      }
    }
  }

  /** Compiles shaders. */
  private def compileShaders() {
    val shaderDir = "./shaders"
    val shaders = new File(shaderDir).listFiles(new FilenameFilter {
      def accept(dir: File, name: String) = name.endsWith(".sl")
    }).map(_.getName)
    for (shaderFileName <- shaders) {
      val process: Process = {
        val cmdArray: Array[String] = Array("aqsl", shaderFileName)
        val dir: File = new File(shaderDir).getCanonicalFile
        Runtime.getRuntime().exec(cmdArray, null, dir)
      }
      val errReader: BufferedReader = new BufferedReader(
        new InputStreamReader(process.getErrorStream))
      process.waitFor
      val errString = if (errReader.ready) {
        val sb = new StringBuffer
        while (errReader.ready) {
          sb.append(errReader.readLine)
        }
        sb.toString
      } else {
        ""
      }
      errReader.close
      if (errString != "") {
        println("Error compiling shader \"%s\":" format shaderFileName)
        println(errString)
      }
    }
  }
  
}
