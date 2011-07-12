package skintwitch.analysis

import scala.collection.immutable._
import scala.xml._
import java.io.File
import java.io.FilenameFilter
import java.io.FileFilter
import RichFile._

trait TrialInput {
  val inputFile: File
  val pointerFile: File
  val horse: String
  val trialNumber: Int
  val site: String
  val start: Option[Int]
  val end: Option[Int]
}

object InputMarshalling {

  /** Internal case class for trial input. */
  private case class IMTrialInput(
    inputFile: File, pointerFile: File, horse: String, trialNumber: Int,
    site: String, start: Option[Int] = None, end: Option[Int] = None
  ) extends TrialInput
  
  /** Find all trials that are within, or in sub-directories of, a top level
   *  directory.
   *  
   *  @param dir directory from which to fetch trials
   *  @return sequence of trials */
  def getTrials(dir: File): Seq[TrialInput] = {    
    // find out if there is a single XML file in this directory; if so, that 
    //  will be the file referencing all trials for a particular horse
    val curDirTrials = {
      val xmlFile = dir.filterDirByString(_.toLowerCase.endsWith(".xml"))
      if (xmlFile.length == 1) {
        readTrialsFromXMLFile(xmlFile.head)
      } else if (xmlFile.length > 1) {
        throw new IllegalArgumentException("More than one XML file found!")
      } else {
        Seq.empty[TrialInput]  // No XML file found
      }
    }
    
    // now fetch trials from all sub-directories
    curDirTrials ++ dir.subDirs.map(getTrials(_)).flatten
  }
  
  /** Read trials from an XML file.
   * 
   *  @param xmlFile XML file from which to read the files
   *  @return sequence of trials that have been read */
  private def readTrialsFromXMLFile(xmlFile: File): Seq[TrialInput] = {
    // load the XML file
    val top = XML.loadFile(xmlFile)
    
    // fetch the horse identifier
    val horseIdentifier = (top \ "@identifier").text
    
    // get all TRC files from the parent directory
    val dir = xmlFile.parent
    val trcFiles = dir.filterDirByString(_.toLowerCase.endsWith(".trc"))
    val trcFileNames = trcFiles.map(_.getName)
    
    // find the pointer file
    val pointerFile = {
      val cand = trcFileNames.filter(_.contains("pointer"))
      if (cand.length == 1) {
        new File(dir, cand.head)
      } else {
        throw new IllegalArgumentException("No unique pointer file found " +
          "for horse %s." format horseIdentifier)
      }
    }
    
    // process trials listed within the XML file
    for (trial <- (top \ "Trial")) yield {
      // fetch trial number and location from XML
      val trialNum = (trial \ "@number").text
      val trialLoc = (trial \ "@loc").text
      val trialStart = {
        val startCand = (trial \ "@start").text
        if (startCand == "") None
        else Some(startCand.toInt)
      }
      val trialEnd = {
        val endCand = (trial \ "@end").text
        if (endCand == "") None
        else Some(endCand.toInt)
      }
      
      // create the trial name and a "joined" trial name
      val trialName = "%s_trials%s" format (horseIdentifier, trialNum)
      val baseName = "%s.trc" format trialName
      val joinName = "%sj.trc" format trialName
      // choose base name or joined file name; preferring joined file name
      //  if it exsits
      val fileName = if (trcFileNames.contains(joinName)) {
        joinName
      } else if (trcFileNames.contains(baseName)) {
        baseName
      } else {
        throw new IllegalArgumentException("Trial %s for horse %s not found!".
          format(trialNum, horseIdentifier))
      }
      val inFile = new File(dir, fileName)
      
      // create trial input object
      IMTrialInput(inFile, pointerFile, horseIdentifier, trialNum.toInt, 
                   trialLoc, trialStart, trialEnd)
    }
  }
  
}
