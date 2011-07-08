package skintwitch.analysis

import scala.collection.immutable._
import java.io.File
import java.io.FileFilter
import java.io.FilenameFilter

/** A rich file class with some useful extra operations. */
case class RichFile(file: File) {
  
  /** Filters a directory using a function which takes a `File` and
   *  returns a `Boolean`. */
  def filterDirByFile(filter: File => Boolean): Seq[File] = {
    assert(file.isDirectory)
    file.listFiles(new FileFilter {
      def accept(pathname: File) = filter(pathname)
    }).toList
  }
  
  /** Filters a directory using a function which takes a `String` (the file
   *  name) and returns a `Boolean. */
  def filterDirByString(filter: String => Boolean): Seq[File] = {
    assert(file.isDirectory)
    file.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) = filter(name)
    }).toList
  }
  
  /** Returns the sub-directories of a `File`. */
  def subDirs(): Seq[File] = filterDirByFile(_.isDirectory)

  /** Returns the parent directory of a file or directory. */
  def parent(): File = file.getParentFile()
  
}

object RichFile {
  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
}