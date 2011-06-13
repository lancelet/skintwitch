package skintwitch

import net.miginfocom.swing.MigLayout
import java.awt.{ BorderLayout, Dimension }
import javax.swing.{ ImageIcon, JPanel, JToolBar, SwingUtilities }
import scala.swing._
import vtk.{ vtkRenderWindowPanel, vtkInteractorStyleTrackballCamera }

class VtkStrainViz {
  assert(SwingUtilities.isEventDispatchThread)
  VtkLoadLibrary.vtkLoadLibraries()
  
  // playback rate field
  val playRateField = new TextField("1.0")
  
  // play button
  val playBtn = new Button {
    borderPainted = false
    icon = new ImageIcon(getClass.getResource("play_up.png"))
    pressedIcon = new ImageIcon(getClass.getResource("play_down.png"))
  }

  // time slider
  val timeSlider = new Slider
  
  // toolbar of the application
  val toolbar = new JToolBar {
    add(new JPanel(new MigLayout("fillx, insets 0 0 0 0",
        "[grow 0][fill, min:pref:60][grow 0][fill]")) {
      add(new Label("Playback rate:").peer)
      add(playRateField.peer)
      add(playBtn.peer)
      add(timeSlider.peer)
    })
  }
  
  // vtk panel
  val vtkPanel = new vtkRenderWindowPanel {
    setMinimumSize(new Dimension(0, 0))
    setPreferredSize(new Dimension(0, 0))    
    setInteractorStyle(new vtkInteractorStyleTrackballCamera)
  }
  val vtkHostPanel = new JPanel(new BorderLayout()) {
    add(vtkPanel, BorderLayout.CENTER)
  }
  
  // main Frame (main window) of the application
  val mainFrame = new Frame {
    title = "VtkStrainViz"
    override def closeOperation = { super.closeOperation; System.exit(0) }
    contents = new BorderPanel {
      //add(toolbar, BorderPanel.Position.NORTH)
      peer.add(toolbar, BorderLayout.NORTH)
      peer.add(vtkHostPanel, BorderLayout.CENTER)
    }
  }
  
  mainFrame.pack
  mainFrame.visible = true
}

/** Execute VtkStrainViz on the AWT Event Dispatch Thread. */
object VtkStrainViz {
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