package skintwitch.analysis

import scala.collection.immutable._
import mocaputils.TRCReader
import skintwitch.MarkerGrid
import mocaputils.GapFiller
import mocaputils.Marker
import java.io.File
import mocaputils.VirtualMarker
import skintwitch.Grid
import skintwitch.Mat2
import signal.Butter
import signal.FiltFilt

/** A trial; encompassing much of the processing required for an individual
 *  trial.
 *  
 *  @param in input trial data
 *  @param refSampleOverride optional override of the reference sample index
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
  i1cutoffFreq: Double = 1.0,
  threshold: Double = 10.0,
  backoffTime: Double = 0.25
) {

  import Trial._
  
  // a unique identifier string for this trial
  val idString = "%s_trial%s" format (in.horse, in.trialNumber)
  
  // load and filter markers from the main trial
  private def markers: Seq[Marker] = {
    require(m_markers != null)
    m_markers
  }
  private var m_markers = loadMarkers(in.inputFile).map(_.butter2(cutoffFreq))
  
  // create the marker grid
  private def markerGrid: MarkerGrid = {
    require(m_markerGrid != null)
    m_markerGrid
  }
  private var m_markerGrid: MarkerGrid = MarkerGrid.fromCRMarkers(markers)

  // set number of samples in the trial and the sampling frequency (just
  //  fetch them from the first marker for the grid; they have to be the
  //  same for all markers)
  val nSamples: Int = markerGrid(0, 0).co.length
  val fs: Double = markerGrid(0, 0).fs
  assert(markers.forall(m => m.co.length == nSamples && m.fs == fs))
  
  // create the virtual marker for the pointer tip
  private def pointer: Marker = {
    require(m_pointer != null)
    m_pointer
  }
  private var m_pointer = createPointerTipMarker(in.pointerFile, markers)

  // distanceAnnotated is the distance from the pointer tip to the grid,
  //  and a second tuple element indicating whether the pointer at the
  //  computed distance lies "within" the grid
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
  
  // the reference sample (just before the poke occurs).  for Control trials,
  //  the reference sample is 0, while for Girthline trials, the reference
  //  sample is specified by `start`.
  val refSample: Int = refSampleOverride.getOrElse {
    if (in.site == "Control") {
      // take sample 0 as the reference in a control trial
      0
    } else if (in.site == "Girthline") {
      // take start as the reference in a girthline trial
      in.start.get // this must be defined for girthline
    } else {
      // the minimum distance that the pointer reaches within the grid
      val minDistance: Double = {
        val withinGridDist = distanceAnnotated.filter(_._2).map(_._1)
        assert(withinGridDist.length > 0)
        withinGridDist.min
      }
      // compute first time that the pointer reaches the given
      //  threshold, and then back off a certain number of samples
      val backOffSamples = (backoffTime * fs).toInt
      val crossing = distanceAnnotated.indexWhere(
        (da: (Double, Boolean)) => {
          da._2 && da._1 <= (minDistance + threshold)
        }
      )
      val cand = crossing - backOffSamples
      if (cand < 0) {
        0
      } else if (cand >= nSamples) {
        nSamples - 1
      } else {
        cand
      }
    }
  }
  assert(refSample >= 0 && refSample < nSamples)

  // find the poke location in st parametric coordinates.  there is no
  //  poke location for control trials and girthline trials
  val pokeLocation: Option[(Double, Double)] = {
    if (in.site == "Control" || in.site == "Girthline") {
      None
    } else {
      val mesh = markerGrid.diceToTrimesh(refSample)
      val (distance, xPoint, st) = mesh.signedDistanceTo(pointer.co(refSample))
      Some(st)
    }
  }
  
  // find the stroke path for a Girthline trial.  the path is a set of st
  //  parametric coordinates describing the path of the tip marker during the
  //  period it is in contact with the skin
  val strokePath: Option[Seq[(Double, Double)]] = {
    if (in.site == "Girthline") {
      val start = in.start.get  // this must be defined for Girthline
      val end   = in.end.get    // this must be defined for Girthline
      val coords = for {
        i <- start until end
        mesh = markerGrid.diceToTrimesh(i)
        (distance, xPoint, st) = mesh.signedDistanceTo(pointer.co(i))
      } yield st
      Some(coords)
    } else {
      None
    }
  }
  
  // compute the grid average of the first invariant of the left Cauchy-Green 
  //  Deformation tensor at each time sample.  this is computed over all
  //  samples, but only the samples after the poke are relevant.
  val i1: IndexedSeq[Double] = for {
    i <- 0 until nSamples
  } yield markerGrid.avgLCauchyGreenI1(refSample, i)
  
  // perform some extra filtering on i1, for the purpose of finding its
  //  initial peak value
  val i1Filt: IndexedSeq[Double] = {
    val sos = Butter.butterSOSEven(2, i1cutoffFreq / (fs / 2)).head
    val b = IndexedSeq(sos.b0, sos.b1, sos.b2)
    val a = IndexedSeq(   1.0, sos.a1, sos.a2)
    FiltFilt.filtfilt(b, a, i1)
  }
  
  // compute the maximum response sample.  this is the first peak in the
  //  i1 values following the poke
  val maxResponseSample: Int = {
    // i1dot is the finite difference of successive i1 values
    val i1dot = for {
      (x0, x1) <- i1Filt zip (i1Filt.tail)
      delta = x1 - x0
    } yield delta
    // firstPositive is when i1dot first becomes positive after the reference
    //  sample
    val firstPositive = i1dot.indexWhere(_ > 0, refSample)
    // now find where i1dot becomes negative after firstPositive
    val maxCandidate = i1dot.indexWhere(_ < 0, firstPositive)
    if (maxCandidate == -1) {
      println("WARNING: Peak not found; setting maxResponseSample to end " +
      		"for trial %s, site %s." format(idString, in.site))
      println("         (This may not be a problem for Girthline trials.)")
      nSamples - 1
    } else {
      maxCandidate
    }
  }
  assert(maxResponseSample > refSample && maxResponseSample < nSamples)
  
  // compute the Biot strain tensor in 2D, at the maximum response sample,
  //  relative to the reference sample
  val biot2d: Grid[Mat2] = markerGrid.biot2D(refSample, maxResponseSample)

  // clean up things we don't reference outside (may need to make some of
  //  these public later, in which case we can't get rid of them)
  m_markers = null
  m_markerGrid = null
  m_pointer = null
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
