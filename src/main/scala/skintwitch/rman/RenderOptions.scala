package skintwitch.rman

import java.awt.BorderLayout
import java.io.{File, FileWriter}
import javax.swing.JPanel
import javax.swing.filechooser.FileNameExtensionFilter
import net.liftweb.json.{DefaultFormats, Serialization, parse}
import net.miginfocom.swing.MigLayout
import scala.io.Source
import scala.swing.{BorderPanel, BoxPanel, Button, CheckBox, Dialog, Label, 
  Orientation, FileChooser, TextField}
import scala.swing.event.ButtonClicked
import vtk.vtkCamera

/** Rendering options. */
case class RenderOptions(trialFileName: String,
  camParams: CameraParams,
  xRes: Int, yRes: Int,
  xSamples: Int, ySamples: Int,
  renderMarkers: Boolean,
  renderSkinGrid: Boolean,
  renderHair: Boolean,
  renderTensors: Boolean)

object RenderOptions {

  /** Displays a dialog box to get render options.
   *  
   *  @param camera VTK camera to use for the render options
   *  @param trialFileName file name of the trial 
   *  @return optional RenderOptions (None if the user cancels the operation) 
   */
  def getRenderOptionsFromDialog(camera: vtkCamera, trialFileName: String): 
  Option[RenderOptions] = {
    var renderOptions: RenderOptions = null
    val xresolution = new TextField("1920")
    val yresolution = new TextField("1080")
    val xsamples = new TextField("6")
    val ysamples = new TextField("6")
    val renderMarkers = new CheckBox("Render markers")
    val renderSkinGrid = new CheckBox("Render skin grid")
    val renderHair = new CheckBox("Render hair")
    val renderTensors = new CheckBox("Render tensors")
    val btnOK = new Button("OK")
    val btnCancel = new Button("Cancel")
    val resSamplePanel = new JPanel(new MigLayout("insets n n 0 n")) {
      // Resolution
      add(new Label("Resolution").peer, "align right")
      add(xresolution.peer, "growx")
      add(new Label("x").peer)
      add(yresolution.peer, "growx,wrap")
      // Samples
      add(new Label("Samples").peer, "align right")
      add(xsamples.peer, "growx")
      add(new Label("x").peer)
      add(ysamples.peer, "growx,wrap")
    }
    val checkboxPanel = new JPanel(new MigLayout("fillx,insets n n 0 n")) {
      add(renderMarkers.peer, "growx,wrap")
      add(renderSkinGrid.peer, "growx,wrap")
      add(renderHair.peer, "growx,wrap")
      add(renderTensors.peer, "growx,wrap")
    }
    val buttonPanel = new JPanel(new MigLayout("fillx")) {
      add(btnOK.peer)
      add(btnCancel.peer)
    }
    val topPanel = new BoxPanel(Orientation.Vertical) {
      peer.add(resSamplePanel)
      peer.add(checkboxPanel)
      peer.add(buttonPanel)
    }
    val dialog = new Dialog { self =>
      title = "Render Options"
      contents = new BorderPanel { 
        peer.add(topPanel.peer, BorderLayout.CENTER) 
      }
      resizable = false
      modal = true
      listenTo(btnOK)
      listenTo(btnCancel)
      reactions += {
        case ButtonClicked(`btnOK`) => {
          try {
            renderOptions = new RenderOptions(
              trialFileName, CameraParams(camera),
              xresolution.text.toInt, yresolution.text.toInt,
              xsamples.text.toInt, ysamples.text.toInt,
              renderMarkers.selected,
              renderSkinGrid.selected,
              renderHair.selected,
              renderTensors.selected
            )
            self.visible = false
          } catch {
            case e: NumberFormatException => {
              renderOptions = null
              Dialog.showMessage(message = "A numeric value is invalid.",
                                 title = "Re-try",
                                 messageType = Dialog.Message.Error)
            }
          }
        }
        case ButtonClicked(`btnCancel`) => {
          renderOptions = null
          self.visible = false
        }
      }
    }
    dialog.pack
    dialog.visible = true
    if (renderOptions != null) Some(renderOptions) else None
  }

  /** Saves render options to a file in JSON format.
   *
   *  @param fileName name of the file
   *  @param renderOptions render options to save
   */
  def saveRenderOptions(fileName: String, renderOptions: RenderOptions) {
    implicit val formats = DefaultFormats
    val fileWriter = new FileWriter(fileName)
    fileWriter.write(Serialization.write(renderOptions))
    fileWriter.close
  }

  /** Loads render options from a file in JSON format.
   *
   *  @param fileName name of the file to read
   *  @return render options loaded from the file
   */
  def loadRenderOptions(fileName: String): RenderOptions = {
    implicit val formats = DefaultFormats
    val source = Source.fromFile(fileName)
    val renderOptions = parse(source.mkString).extract[RenderOptions]
    source.close
    renderOptions
  }

  /** Shows a render options dialog box, then a save file dialog box,
   *  saving the render options.
   *
   *  @param camera vtk camera to save to file with the render options
   *  @param trialFileName name of the trial file
   */
  def getRenderOptionsFromDialogAndSave(camera: vtkCamera,
    trialFileName: String) {
    // display a render options dialog box
    val renderOptions = getRenderOptionsFromDialog(camera, trialFileName)
    if (renderOptions.isEmpty) return
    // show save file dialog box
    val fc = new FileChooser(new File(".")) {
      fileFilter = new FileNameExtensionFilter("JSON files", "json")
    }
    if (fc.showSaveDialog(null) != FileChooser.Result.Approve) return
    val fileName = fc.selectedFile.getCanonicalFile.getPath
    // save the file
    saveRenderOptions(fileName, renderOptions.get)
  }

}
