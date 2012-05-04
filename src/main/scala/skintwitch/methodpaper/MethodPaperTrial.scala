package skintwitch.methodpaper

import scala.collection.immutable._
import java.io.File
import mocaputils.{Vec3 => MVec3}
import mocaputils.GapFiller
import mocaputils.Marker
import mocaputils.TRCReader
import mocaputils.VirtualMarker
import skintwitch.MarkerGrid
import skintwitch.Mat3
import skintwitch.analysis.TrialInput
import skintwitch.Grid
import skintwitch.BicubicInterpGrid
import skintwitch.Mat2
import skintwitch.Vec2
import skintwitch.Vec3
import skintwitch.mesh.MeshDistance


/** Analysis of a single trial for the method paper. 
  * 
  * @param in input trial data
  * @param refSampleOverride optional override of the reference sample index
  * @param cutoffFreq cutoff frequency at which to filter the trial (low-pass,
  *   forward-reverse, second-order Butterworth filter) (in Hz)
  * @param threshold value to add to the minimum distance that the marker
  *   enters into the grid, in order to find the poke event (in mm)
  * @param backoffTime amount of time to back off from the poke event time
  *   in order to find the reference sample (in seconds) */
case class MethodPaperTrial(
  in: TrialInput,
  refSampleOverride: Option[Int] = None,
  cutoffFreq: Double = 5.0,
  threshold: Double = 12.0,
  backoffTime: Double = 0.0
) {

  //--------------------------------------------------------------------------
  // form unique identifier string for the trial
  val idString = "%s_trial%s" format (in.horse, in.trialNumber)
  
  //--------------------------------------------------------------------------
  // load markers and low-pass filter the marker positions from the trial
  private def markers: Seq[Marker] = {
    assert(m_markers != null)
    m_markers
  }
  private var m_markers = {
    import MethodPaperTrial.loadMarkers
    loadMarkers(in.inputFile).map(_.butter2(cutoffFreq))
  }
  
  //--------------------------------------------------------------------------
  // create marker grid
  private def markerGrid: MarkerGrid = {
    assert(m_markerGrid != null)
    m_markerGrid
  }
  private var m_markerGrid: MarkerGrid = MarkerGrid.fromCRMarkers(markers)
  
  //--------------------------------------------------------------------------
  // extract number of samples and sampling frequency.  we just fetch them
  //  from the first marker of the grid, since they have to be the same for
  //  all markers.
  val nSamples: Int = markerGrid(0, 0).co.length
  val fs: Double = markerGrid(0, 0).fs
  assert(markers.forall(m => m.co.length == nSamples && m.fs == fs)) // check
  
  //--------------------------------------------------------------------------
  // create virtual marker for the pointer tip
  private def pointer: Marker = {
    assert(m_pointer != null)
    m_pointer
  }
  private var m_pointer: Marker = {
    import MethodPaperTrial.createPointerTipMarker
    createPointerTipMarker(in.pointerFile, markers)
  }
  
  //--------------------------------------------------------------------------
  // distanceAnnotated is the distance from the pointer tip to the grid,
  //  and a second tuple element indicating whether the pointer at the
  //  computed distance lies "within" the grid
  val distance: Seq[MeshDistance] = {
    for {
      i <- 0 until nSamples
      mesh = markerGrid.diceToTrimesh(i)
      pointerPoint = Vec3(pointer.co(i))
    } yield mesh.distanceTo(pointerPoint)
  }
  
  //--------------------------------------------------------------------------
  // the reference sample (when the poke occurs).  for Control trials,
  //  this is 0, while for Girthline trials, the reference sample is specified
  //  explicitly by 'start'.  the reference sample can be overridden by
  //  using the refSampleOverride parameter.
  val refSample: Int = refSampleOverride.getOrElse {
    in.site match {
      case "Control" => 0 // sample 0 is the reference for control trials
      case "Girthline" => in.start.get // must be defined for girthline trials
      case _ => {
        // minimum distance the pointer reaches within the grid
        val minDistance: Double = {
          val withinGridDist = distance.filter(_.stInGrid()).map(_.distance)
          assert(withinGridDist.length > 0)
          withinGridDist.min
        }        
        // find when the pointer reaches `threshold`, and then back off
        //  `backoffTime`
        val backoffSamples = (backoffTime * fs).toInt
        val crossing = distance.indexWhere(md => {
          md.stInGrid() && (md.distance <= (minDistance + threshold))
        })
        MethodPaperTrial.clamp(crossing - backoffSamples, 0, nSamples-1)
      }
    }
  }
  assert(refSample >= 0 && refSample < nSamples) // sanity check range

  //--------------------------------------------------------------------------
  // compute the grid average of the first invariant of the left Cauchy-Green 
  //  Deformation tensor at each time sample.  this is computed over all
  //  samples, but only the samples after the poke are relevant.
  val i1: IndexedSeq[Double] = for {
    i <- 0 until nSamples
  } yield markerGrid.avgLCauchyGreenI1(refSample, i)

  //--------------------------------------------------------------------------
  // find the sample where the maximum twitch response occurs.  this is the
  //  first peak in the i1 values following the poke.
  val maxResponseSample: Int = {
    // finite difference of successive i1 values
    val i1dot = i1.zip(i1.tail).map(x => x._2 - x._1)
    // firstPositive is when i1dot first becomes positive after the reference
    val firstPositive = i1dot.indexWhere(_ > 0, refSample)
    // new find where i1dot becomes negative after firstPositive
    val maxCandidate = i1dot.indexWhere(_ < 0, firstPositive)
    assert(maxCandidate > refSample && maxCandidate < nSamples)
    maxCandidate
  }
  
  //--------------------------------------------------------------------------
  // find the minimum principal strain (compressive) with the largest
  //  magnitude at the maximum response sample.  here we interpolate the grid
  //  at 10x its original resolution.
  val minPrinStrainAtMaxResponse: Double = {
    val biotGrid = markerGrid.biot(refSample, maxResponseSample)
    
    def minPS(m: Mat3): Double = m.eigSymm.map(_._1).min
    val newRows = biotGrid.numRows * 10
    val newCols = biotGrid.numCols * 10
    val minPSGrid = BicubicInterpGrid(biotGrid.map(minPS)).
        toGrid(newRows, newCols)
    
    minPSGrid.rowMajor.min
  }
  
  //--------------------------------------------------------------------------
  // find the I1 grid (the first invariant of the Left Cauchy-Green
  //  deformation tensor) at the maximum response
  val i1AtMaxResponse: Grid[Double] = 
    markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
  
  //--------------------------------------------------------------------------
  // interpolate the I1 grid at 10x resolution
  val i1AtMaxResponseInterp: Grid[Double] =
    BicubicInterpGrid(i1AtMaxResponse).toGrid(
            i1AtMaxResponse.numRows * 10,
            i1AtMaxResponse.numCols * 10
        )
    
  //--------------------------------------------------------------------------
  // find the UV coordinates of the maximum I1 (first invariant of the Left
  //  Cauchy-Green deformation tensor). here we interpolate the grid at 10x
  //  its original resolution
  val maxI1AtMaxResponseUVCoords: Vec2 =
    i1AtMaxResponseInterp.maxUV
  
  //--------------------------------------------------------------------------
  // find the poke location (U, V) in grid coordinates.  there is no poke 
  //  location for control trials and girthline trials.  U is a column-linked
  //  variable, V is a row-linked variable.
  // find the poke location in st parametric coordinates.  there is no
  //  poke location for control trials and girthline trials
  val pokeLocation: Option[Vec2] = {
    if (in.site == "Control" || in.site == "Girthline") {
      None
    } else {
      val mesh = markerGrid.diceToTrimesh(refSample)
      val meshDistance = mesh.distanceTo(Vec3(pointer.co(refSample)))
      Some(meshDistance.st)
    }
  }
    
  //--------------------------------------------------------------------------
  // compute the Biot strain grid for this trial
  val biot2d: Grid[Mat2] = markerGrid.biot2D(refSample, maxResponseSample)
  
  //--------------------------------------------------------------------------
  // compute the I1 grid for this trial
  val i1Grid: Grid[Double] = 
    markerGrid.lCauchyGreenI1(refSample, maxResponseSample)

}


object MethodPaperTrial {
  
  /** Load markers from a TRC file.  Masks out any empty markers and force-fill
    * the rest.
    *
    * @param inFile input file
    * @return sequence of Markers. */
  def loadMarkers(inFile: File): Seq[Marker] = {
    // load TRC file data
    val trcData = TRCReader.read(inFile.getCanonicalPath).fold (
      e => throw new IllegalArgumentException("Could not read TRC file %s".
          format(inFile.getCanonicalPath)),
      s => s
    )
    
    // mask out any empty markers and force-fill the rest
    val egmarkers = trcData.markers.filter(_.exists) // marker must "exist"
    egmarkers.map(GapFiller.fillGapsLerp(_).get)
  }
  
  /** Create a virtual marker for the tip of the pointer / poker for a trial.
    *
    * The markers always visible on the poker are assumed to be named
    * "middle", "long", "med" and "short", while the tip marker of the poker
    * (visible only in the poker calibration trial) is called "T6".
    * 
    * @param pointerFile file containing the poker calibration trial
    * @param trialMarkers sequence of Markers from the trial
    * @return virtual marker for the pointer tip in the trial */
  def createPointerTipMarker(pointerFile: File, trialMarkers: Seq[Marker]):
  Marker = {
    // load markers from the static pointer file; low-pass filter at 1 Hz
    val pStatic = loadMarkers(pointerFile).map(_.butter2(1.0))
    
    // reference sample for the static trial (just use mid-trial)
    val nRefSample = pStatic(0).co.length / 2
    // finds a marker in the static trial
    def s(name: String) = pStatic.find(_.name == name).get.co(nRefSample)
    
    // reference markers (static, dynamic)
    val refMarkers: Seq[Tuple2[MVec3, Marker]] = {
      def d(name: String) = trialMarkers.find(_.name == name).get
      def sd(name: String) = (s(name), d(name))
      Seq("middle", "long", "med", "short").map(sd)
    }
    
    // construct virtual marker (the tip marker is called "T6" in the pointer
    //   static calibration trials)
    new VirtualMarker("tip", s("T6"), refMarkers)
  }
  
  /** Clamps a value.  If x is less than minX, minX is returned.  If x is 
    * greater than maxX then maxX is returned.  Otherwise x is returned.
    *  
    * @param x value to clamp
    * @param minX minimum allowed value
    * @param maxX maximum allowed value
    * @return clamped value */
  def clamp(x: Int, minX: Int, maxX: Int): Int =
    if (x < minX) minX else if (x > maxX) maxX else x
    
}
