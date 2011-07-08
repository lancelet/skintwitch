package skintwitch.analysis

import scala.collection.immutable._
import mocaputils.TRCReader
import skintwitch.MarkerGrid
import mocaputils.GapFiller
import mocaputils.Marker
import java.io.File
import mocaputils.VirtualMarker

/** A trial; encompassing much of the processing require for an individual
 *  trial.
 *  
 *  @param in input trial data
 *  @param refSampleOverride optional override of the reference sample
 *  @param cutoffFreq cutoff frequency at which to filter the trial (low-pass,
 *    forward-reverse, second-order Butterworth filter) (in Hz)
 *  @param threshold value to add to the minimum distance that the marker
 *    enters into the grid, in order to find the poke event (in millimeters)
 *  @param backoffTime amount of time to back off from the poke even time
 *    in order to find the reference sample (in seconds) */
case class Trial(
  in: TrialInput,
  refSampleOverride: Option[Int] = None,
  cutoffFreq: Double = 5.0,
  threshold: Double = 10.0,
  backoffTime: Double = 0.25
) {

  import Trial._
  
  // load and filter markers from the main trial
  val markers = loadMarkers(in.inputFile).map(_.butter2(cutoffFreq))
  
  // create the marker grid
  val markerGrid: MarkerGrid = MarkerGrid.fromCRMarkers(markers)

  // set number of samples in the trial and the sampling frequency (just
  //  fetch them from the first marker for the grid; they have to be the
  //  same for all markers)
  val nSamples: Int = markerGrid(0, 0).co.length
  val fs: Double = markerGrid(0, 0).fs
  assert(markers.forall(m => m.co.length == nSamples && m.fs == fs))
  
  // create the virtual marker for the pointer tip
  val pointer = createPointerTipMarker(in.pointerFile, markers)

  // distanceAnnotated is the distance from the pointer tip to the grid,
  //  with a second tuple element indicating whether the pointer at the
  //  computed distance lies within the grid.
  val distanceAnnotated: Seq[(Double, Boolean)] = {
    def isWithinGrid(st: (Double, Double)): Boolean = {
      val minm = 0.01
      val maxm = 1 - minm
      (st._1 > minm) && (st._1 < maxm) && (st._2 > minm) && (st._2 < maxm)
    }
    for {
      i <- 0 until nSamples
      mesh = markerGrid.diceToTrimesh(i)
      (distance, xPoint, st) = mesh.signedDistanceTo(pointer.co(i))
      inGrid = isWithinGrid(st)
    } yield (distance, inGrid)
  }
  
  // the minimum distance that the pointer reaches within the grid
  val minDistance: Double = {
    val withinGridDist = distanceAnnotated.filter(_._2).map(_._1)
    assert(withinGridDist.length > 0)
    withinGridDist.min
  }

  // the reference sample (just before the poke occurs)
  val refSample: Int = refSampleOverride.getOrElse {
    val backOffSamples = (backoffTime * fs).toInt
    val cand = distanceAnnotated.indexOf(
      (da: (Double, Boolean)) => da._2 && (da._1 <= minDistance + threshold)) -
      backOffSamples
    if (cand < 0) {
      0
    } else if (cand >= nSamples) {
      nSamples - 1
    } else {
      cand
    }  
  }
  
}

object Trial {
  
  /** Loads markers from a TRC file, masks out any empty markers, and
   *  force-fills the rest. */
  def loadMarkers(inFile: File): Seq[Marker] = {
    // load the trc file data
    val trcData = TRCReader.read(inFile.getCanonicalPath).fold(
      e => throw new IllegalArgumentException("Could not read TRC file %s".
        format(inFile.getCanonicalPath)),
      s => s)
    
    // mask out any empty markers and force-fill the rest
    val egmarkers = trcData.markers.filter(_.exists)
    egmarkers.map(GapFiller.fillGapsLerp(_).get)
  }
  
  /** Creates a virtual marker for the tip of the pointer / poker.
   * 
   *  @param pointerFile TRC file containing a static pointer trial
   *  @param trialMarkers markers from the main trial
   *  @return virtual marker for the tip of the pointer during the main trial
   */
  def createPointerTipMarker(pointerFile: File, trialMarkers: Seq[Marker]):
  Marker = {
    // load markers from the static pointer trial; low-pass filter at a very
    //  low frequency
    val pStatic = loadMarkers(pointerFile).map(_.butter2(1.0))
    
    // reference markers (static, dynamic)
    val nStaticSamples = pStatic(0).co.length
    val nRefSample = nStaticSamples / 2  // take sample from mid-trial 
    def s(name: String) = pStatic.find(_.name == name).get.co(nRefSample)
    def d(name: String) = trialMarkers.find(_.name == name).get
    val refMarkers = Seq(
      (s("middle"), d("middle")),
      (s("long"),   d("long")),
      (s("med"),    d("med")),
      (s("short"),  d("short")))
    
    // construct virtual marker (the tip marker is called "T6" in the pointer
    //  static trials)
    new VirtualMarker("tip", s("T6"), refMarkers)
  }
  
}