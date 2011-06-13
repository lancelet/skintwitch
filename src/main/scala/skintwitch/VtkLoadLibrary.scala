package skintwitch

object VtkLoadLibrary {

  /**
   * Loads JNI libraries that VTK depends upon.
   */
  def vtkLoadLibraries() {
    System.loadLibrary("vtkCommonJava")
    System.loadLibrary("vtkFilteringJava")
    System.loadLibrary("vtkIOJava")
    System.loadLibrary("vtkImagingJava")
    System.loadLibrary("vtkGraphicsJava")
    System.loadLibrary("vtkRenderingJava")
  }

}
