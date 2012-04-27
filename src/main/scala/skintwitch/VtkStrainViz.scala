package skintwitch

import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.BorderLayout
import java.awt.Dimension
import java.io.File
import java.io.FileWriter
import java.io.FilenameFilter
import scala.collection.immutable.Seq
import scala.io.Source
import scala.swing.event.ButtonClicked
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.ValueChanged
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.FileChooser
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Slider
import scala.swing.TextField
import VtkStrainViz.JSONCamParams
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.ImageIcon
import javax.swing.JPanel
import javax.swing.JToolBar
import javax.swing.KeyStroke
import javax.swing.SwingUtilities
import javax.swing.Timer
import mocaputils.GapFiller
import mocaputils.TRCReader
import net.liftweb.json.Serialization.write
import net.liftweb.json.parse
import net.liftweb.json.DefaultFormats
import net.miginfocom.swing.MigLayout
import skintwitch.rman.RManRender
import skintwitch.rman.RenderOptions
import skintwitch.vtk.AnimatedActor
import skintwitch.vtk.MarkerGridActor
import skintwitch.vtk.BiotRateActor
import skintwitch.vtk.PointerActor
import vtk.Animated2DActor
import _root_.vtk.vtkInteractorStyleTrackballCamera
import _root_.vtk.vtkRenderWindowPanel
import skintwitch.vtk.FrameActor
import skintwitch.vtk.DistancePlotActor
import scala.swing.Dialog
import java.awt.Cursor
import skintwitch.vtk.I1PlotActor

class VtkStrainViz {
  assert(SwingUtilities.isEventDispatchThread)
  VtkLoadLibrary.vtkLoadLibraries()
  import VtkStrainViz._
  
  // collection (mutable) of AnimatedActors (used so that we can update the
  //  current sample during animation)
  private val actors = scala.collection.mutable.Buffer.empty[AnimatedActor]
  // collection of Animated2DActors
  private val actors2d = scala.collection.mutable.Buffer.empty[Animated2DActor]
  
  // playback rate field
  private val playRateField: TextField = new TextField("1.0") {
    listenTo(this.keys)
    reactions += {
      // when Enter is pressed, update the timer
      case KeyPressed(_, Key.Enter, _, _) => {
        val p: Double = try {
          playRateField.text.toDouble
        } catch {
          case e: NumberFormatException => 1.0
        }
        playRateField.text = p.toString
        updateTimer()
      }
    }
  }
  
  // play button
  private val playBtn = new Button {
    borderPainted = false
    enabled = false
    reactions += {
      case ButtonClicked(_) => {
        if (timer.isRunning) timer.stop else timer.start
        updateIcon()
      }
    }
    private val playUp = new ImageIcon(getClass.getResource("play_up.png"))
    private val playDn = new ImageIcon(getClass.getResource("play_down.png"))
    private val pauseUp =
      new ImageIcon(getClass.getResource("pause_up.png"))
    private val pauseDn =
      new ImageIcon(getClass.getResource("pause_down.png"))
    private def updateIcon() {
      icon = if (timer != null && timer.isRunning) pauseUp else playUp
      pressedIcon = if (timer != null && timer.isRunning) pauseDn else playDn
    }
    updateIcon()
  }

  // time slider
  private val timeSlider = new Slider {
    enabled = false
    min = 0
    max = 10
    value = 0
    reactions += {
      case ValueChanged(_) => {
        actors.par.map(_.setSample(value))
        actors2d.par.map(_.setSample(value))
        vtkPanel.GetRenderer().ResetCameraClippingRange()
        vtkPanel.Render()
      }
    }
  }
  
  // timer
  private var trialfps: Double = 0.0
  private var sampleIncrement: Int = 1
  private val timer = new Timer(1000, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (timeSlider.value == timeSlider.max) {
        timeSlider.value = 0
      } else {
        timeSlider.value = timeSlider.value + sampleIncrement
      }
    }
  })
  timer.setInitialDelay(0)
  private def updateTimer() {
    val playRate = playRateField.text.toDouble
    var delay = 1
    sampleIncrement = -1
    while (delay < 30) {  // don't try to run faster than 30fps
      sampleIncrement += 1
      delay = (1000 * sampleIncrement / trialfps / playRate).toInt
    }
    timer.setDelay(delay)
  }
  
  // toolbar of the application (use MigLayout to make it a bit nicer)
  private val toolbar = new JToolBar {
    add(new JPanel(new MigLayout("fillx, insets 0 0 0 0",
        "[grow 0][fill, min:pref:60][grow 0][fill]")) {
      add(new Label("Playback rate:").peer)
      add(playRateField.peer)
      add(playBtn.peer)
      add(timeSlider.peer)
    })
  }
  
  // action to open a TRC file
  private var curDir: Option[File] = None
  private val openTrcAction = new Action("Open *.TRC file...") {
    accelerator = Some(KeyStroke.getKeyStroke("meta O"))
    def apply() {
      val fc = new FileChooser(curDir.getOrElse(new File("."))) {
        fileFilter = new FileNameExtensionFilter("TRC files", "trc")
      }
      if (fc.showOpenDialog(null) == FileChooser.Result.Approve) {
        curDir = Some(fc.selectedFile.getParentFile)
        openTrcFile(fc.selectedFile.getCanonicalFile.getPath)
      }
    }
  }
  
  // action to render the current frame using Aqsis
  private val renderFrameAction = new 
  Action("Render current frame using Aqsis") {
    accelerator = Some(KeyStroke.getKeyStroke("meta R"))
    def apply() {
      val camera = vtkPanel.GetRenderer.GetActiveCamera
      RManRender.renderFrame(timeSlider.value, grid, camera)
    }
  }
  
  // action to render the entire animation using Aqsis
  private val renderAllAction = new
  Action("Render animation using Aqsis") {
    def apply() {
      val camera = vtkPanel.GetRenderer.GetActiveCamera
      RManRender.renderAnim(grid, camera)
    }
  }
  
  // action to export render options
  private val exportRenderOptions = new Action("Export render options...") {
    def apply() {
      val camera = vtkPanel.GetRenderer.GetActiveCamera
      val trialName = canonicalTrialName
      RenderOptions.getRenderOptionsFromDialogAndSave(camera, trialName)
    }
  }
  
  /** Saves camera parameters to a JSON file. */
  private val saveCameraParams = new Action("Save camera parameters...") {
    def apply() {
      // show file dialog
      val fc = new FileChooser(new File(".")) {
        fileFilter = new FileNameExtensionFilter("JSON files", "json")
      }
      if (fc.showSaveDialog(null) != FileChooser.Result.Approve) {
        return
      }
      val fileName = fc.selectedFile.getCanonicalFile.getPath
    
      // fetch parameters in JSON format
      implicit val formats = DefaultFormats
      val camera = vtkPanel.GetRenderer.GetActiveCamera
      val camParam = JSONCamParams(
        camera.GetViewAngle, camera.GetRoll,
        camera.GetFocalPoint, camera.GetPosition)
      val camJSON = write(camParam)
    
      // save parameters
      val fw = new FileWriter(fileName)
      fw.write(camJSON)
      fw.close
    }
  }
  
  /** Reads camera parameters from a JSON file. */
  private val readCameraParams = new Action("Read camera parameters...") {
    def apply() {
      // show file dialog
      val fc = new FileChooser(new File(".")) {
        fileFilter = new FileNameExtensionFilter("JSON files", "json")
      }
      if (fc.showOpenDialog(null) != FileChooser.Result.Approve) {
        return
      }
      val fileName = fc.selectedFile.getCanonicalFile.getPath

      // read the file / parse JSON
      implicit val formats = DefaultFormats
      val json = parse(Source.fromFile(fileName).mkString)
      val cp = json.extract[JSONCamParams]
      
      // configure the camera
      val camera = vtkPanel.GetRenderer.GetActiveCamera
      camera.SetViewAngle(cp.viewAngle)
      camera.SetFocalPoint(cp.focalPoint)
      camera.SetPosition(cp.position)
      camera.SetRoll(cp.roll)
      vtkPanel.GetRenderer().ResetCameraClippingRange()
      vtkPanel.Render()
    }
  }
  
  
  // menu bar
  private val topMenuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(openTrcAction)
      contents += new MenuItem(saveCameraParams)
      contents += new MenuItem(readCameraParams)
    }
    contents += new Menu("Render") {
      contents += new MenuItem(renderFrameAction)
      contents += new MenuItem(renderAllAction)
      contents += new MenuItem(exportRenderOptions)
    }
  }
  
  // vtk panel
  private lazy val vtkPanel = new vtkRenderWindowPanel {
    setMinimumSize(new Dimension(0, 0))
    setPreferredSize(new Dimension(0, 0))    
    setInteractorStyle(new vtkInteractorStyleTrackballCamera)
    GetRenderWindow.LineSmoothingOn
    GetRenderWindow.PolygonSmoothingOn
    GetRenderWindow.SetAAFrames(16)
    GetRenderer.SetBackground(1, 1, 1)
  }
  private val vtkHostPanel = new JPanel(new BorderLayout()) {
    add(vtkPanel, BorderLayout.CENTER)
  }
  
  // main Frame (main window) of the application
  private val mainFrame = new Frame {
    title = "VtkStrainViz"
    override def closeOperation = { super.closeOperation; System.exit(0) }
    menuBar = topMenuBar
    contents = new BorderPanel {
      //add(toolbar, BorderPanel.Position.NORTH)
      peer.add(toolbar, BorderLayout.NORTH)
      peer.add(vtkHostPanel, BorderLayout.CENTER)
    }
  }
    
  /** Opens a TRC file. */
  private var grid: MarkerGrid = null
  private var trialName: String = null
  private var canonicalTrialName: String = null
  private def openTrcFile(fileName: String) {
    // read the trial TRC data
    val trcData = TRCReader.read(fileName).fold (
      e => {
        Dialog.showMessage(null,
          "Error opening file \"%s\"\n%s" format (fileName, e),
          "Error", Dialog.Message.Error)
        return
      },
      s => s
    )
    
    // set a busy cursor
    mainFrame.cursor = new Cursor(Cursor.WAIT_CURSOR)
    
    // set the name of the trial
    canonicalTrialName = new File(fileName).getCanonicalPath
    trialName = new File(fileName).getName.dropRight(4)
    mainFrame.title = "VtkStrainViz - %s" format trialName
    
    // find and load the pointer trial
    val path = new File(fileName).getParentFile
    val ptrialFiles = path.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) = name.contains("pointer")
    })
    if (ptrialFiles.length > 1) {
      Dialog.showMessage(null,
        "More than one pointer trial found.",
        "Error", Dialog.Message.Error)
      mainFrame.cursor = new Cursor(Cursor.DEFAULT_CURSOR)
      return
    } else if (ptrialFiles.length == 0) {
      Dialog.showMessage(null,
        "No pointer trial found.",
        "Error", Dialog.Message.Error)
      mainFrame.cursor = new Cursor(Cursor.DEFAULT_CURSOR)
    }
    val pTrialTrc = TRCReader.read(ptrialFiles.head.getCanonicalPath).fold (
      e => {
        Dialog.showMessage(null,
          "Error opening file \"%s\"\n%s".
            format(ptrialFiles.head.getCanonicalPath, e),
          "Error", Dialog.Message.Error)
        mainFrame.cursor = new Cursor(Cursor.DEFAULT_CURSOR)
        return
      },
      s => s
    )
    
    trialfps = trcData.cameraRate
    updateTimer()
    
    // remove any previous VTK actors
    vtkPanel.GetRenderer.RemoveAllViewProps
    actors.clear
    actors2d.clear
    
    // set up the time slider and play button
    timeSlider.enabled = true
    timeSlider.min = 0
    timeSlider.value = 0
    timeSlider.max = trcData.numFrames - 1
    playBtn.enabled = true
    
    // force fill markers and low-pass Butterworth filter
    val markers = trcData.markers.filter(_.exists).
      map(GapFiller.fillGapsLerp(_).get).map(_.butter2(5.0))
    
    // construct grid from markers
    grid = MarkerGrid.fromCRMarkers(markers)
    
    // add grid to the actors
    val mga = new MarkerGridActor(grid)
    actors += mga
    mga.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // add pointer to the actors
    val pointerStaticGappedMarkers = Seq(
      pTrialTrc.getMarker("middle"), pTrialTrc.getMarker("short"),
      pTrialTrc.getMarker("med"), pTrialTrc.getMarker("long"),
      pTrialTrc.getMarker("T6")
    )
    val pointerStaticMarkers = pointerStaticGappedMarkers.map(
      GapFiller.fillGapsLerp(_).get)
    val pointerActor = new PointerActor(pointerStaticMarkers, markers, grid)
    actors += pointerActor
    pointerActor.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // add Biot rate tensor
    val biotRateActor = new BiotRateActor(grid)
    actors += biotRateActor
    biotRateActor.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // add frame number actor
    val frameActor = new FrameActor
    actors2d += frameActor
    frameActor.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // add distance plot actor
    val distancePlotActor = new DistancePlotActor(pointerStaticMarkers,
                                                  markers, grid,
                                                  vtkPanel.Render)
    actors2d += distancePlotActor
    distancePlotActor.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // add I1 plot actor
    val i1PlotActor = new I1PlotActor(pointerStaticMarkers,
                                      markers, grid,
                                      vtkPanel.Render)
    actors2d += i1PlotActor
    i1PlotActor.getActors.map(vtkPanel.GetRenderer.AddActor(_))
    
    // reset the camera
    vtkPanel.GetRenderer.ResetCamera
    
    // reset to the default cursor
    mainFrame.cursor = new Cursor(Cursor.DEFAULT_CURSOR)
    
    // call for a render
    vtkPanel.Render
  }
  
  mainFrame.size = new Dimension(800, 600)
  mainFrame.visible = true
}

/** Execute VtkStrainViz on the AWT Event Dispatch Thread. */
object VtkStrainViz {
  /** Camera parameters for JSON storage. */
  private case class JSONCamParams(
    viewAngle: Double, roll: Double, focalPoint: Array[Double],
    position: Array[Double]
  )  
  
  def main(args: Array[String]) = {
    // OSX kung-fu (run before Swing is started)
    if (System.getProperty("os.name") == "Mac OS X") {
      System.setProperty("apple.laf.useScreenMenuBar", "true")
      System.setProperty("com.apple.mrj.application.apple.menu.about.name", 
          "VtkStrainViz")
    }
    
    // run from the AWT EDT
    SwingUtilities.invokeLater(new Runnable { def run() = new VtkStrainViz() })
  }
}
