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
import skintwitch.BicubicInterpGrid
import skintwitch.Mat3
import skintwitch.Vec2
import skintwitch.Vec3
import skintwitch.Linearizable
import skintwitch.mesh.MeshDistance

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
  threshold: Double = 12.0,
  backoffTime: Double = 0.0
) {

  import Trial._
  
  //--------------------------------------------------------------------------
  // Create a unique identifier string for this trial.
  //
  // The marker string identifies the horse and the trial number, which
  // uniquely identifies the trial.
  private val idString = "%s_trial%s" format (in.horse, in.trialNumber)
  
  //--------------------------------------------------------------------------
  // Load and filter markers from the main trial.
  //
  // Markers are loaded for the trial.  In this step, the markers are loaded,
  // force-filled (to remove any data gaps), and low-pass filtered at a
  // cut-off frequency which is passed-in to the trial.
  private lazy val markers: Seq[Marker] = 
    loadMarkers(in.inputFile).map(_.butter2(cutoffFreq))

  //--------------------------------------------------------------------------
  // Create the marker grid.
  //
  // The marker grid is the main representation of the marker data.  The
  // marker grid holds all of the markers in a 2D grid.  Each marker holds
  // its own information about its position at a given time sample.
  private lazy val markerGrid: MarkerGrid = MarkerGrid.fromCRMarkers(markers)

  //--------------------------------------------------------------------------
  // Number of samples and sampling rate.
  //
  // We fetch the number of samples and the sampling rate from the first
  // marker, and double-check that they match for all other markers.
  private lazy val nSamples: Int = {
    val n = markerGrid(0, 0).co.length
    assert(markers.forall(_.co.length == nSamples),
        "Markers must all have the same number of samples.")
    n
  }
  private lazy val fs: Double = {
    val fs0 = markerGrid(0, 0).fs
    assert(markers.forall(_.fs == fs0),
        "Markers must all have the same sampling rate.")
    fs0
  }

  //--------------------------------------------------------------------------
  // Create the virtual marker for the pointer tip.
  //
  // The pointer tip marker is constructed as a virtual marker using the
  // cluster of markers on the pointer/poking device.
  private lazy val pointer: Marker = 
    createPointerTipMarker(in.pointerFile, markers)

  //--------------------------------------------------------------------------
  // Distance from the pointer tip to the grid.
  //
  // In order to compute the sample at which the poke occurs (the reference
  // sample), we need to know the minimum distance of the pointer from the
  // grid for all time samples.
  private lazy val distance: Seq[MeshDistance] = {
    for {
      i <- 0 until nSamples
      mesh = markerGrid.diceToTrimesh(i)
      pointerPoint = Vec3(pointer.co(i))
    } yield mesh.distanceTo(pointerPoint)
  }

  //--------------------------------------------------------------------------
  // The reference sample (\mu_0).
  //
  // The reference sample is defined in three different ways:
  //   1. Control trials.  For control trials, the reference sample is just
  //       sample 0.  Since no poking is occurring, we have no better way to
  //       define it, so we might as well just start at the beginning of the
  //       trial.
  //   2. Girthline trials.  For girthline trials, each trial was viewed
  //       by an operator, and a "start" sample was set manually as the time
  //       at which the girthline stroke began.
  //   3. All other trials (T6, T11, T16, G1, G2, G3).  The minimum distance
  //       that the poke reaches into the grid is found first.  Then we find
  //       when the poker crosses some threshold away from this minimum
  //       distance.  Then we back off a pre-defined number of samples
  //       (calculated from backoffTime).
  //
  // The reference sample may be overridden flat-out, cancelling all of the
  // above, by setting the refSampleOverride value.
  private lazy val refSample: Int = refSampleOverride.getOrElse {
    val cRefSample = in.site match {
      case "Control" => 0
      case "Girthline" => in.start.get
      case _ => {
        assert(distance.exists(_.stInGrid()), "Poker never entered the grid!")
        val minDist = distance.filter(_.stInGrid()).map(_.distance).min
        val crossing = distance.indexWhere(md => {
          md.stInGrid() && (md.distance <= (minDist + threshold))
        })
        val s = crossing - (backoffTime * fs).toInt
        if (s < 0) 0 else if (s >= nSamples) nSamples - 1 else s
      }
    }
    assert(cRefSample >= 0 && cRefSample < nSamples, 
        "Reference sample %d was outside the allowed range [%d, %d]" format
         (cRefSample, 0, nSamples-1))
    cRefSample
  }
  
  //--------------------------------------------------------------------------
  // Poke location and distance properties.
  //
  // The poke has certain properties relative to the mesh, which we can
  // establish by looking at the pointer at the reference sample.  The
  // pokeMeshDistance contains the information about:
  //  - where on the skin manifold (in e_i) the poke occurred (_.meshPoint)
  //  - the st coordinates (in the B parametric space) of the poke (_.st)
  private lazy val pokeMeshDistance: Option[MeshDistance] = {
    in.site match {
      case "Control" => None
      case "Girthline" => None
      case _ => {
        val mesh = markerGrid.diceToTrimesh(refSample)
        val p = Vec3(pointer.co(refSample))
        Some(mesh.distanceTo(p))
      }
    }
  }
  
  //--------------------------------------------------------------------------
  // Stroke path for a Girthline trial.
  //
  // Instead of a single poke point, a girthline trial has a path, which
  // consists of the st coordinates (in B parametric space) of the end of the
  // pointer/poker through time.  This path is the path of the tip marker
  // during the period that it was in contact with the skin (from in.start to
  // in.end).
  private lazy val strokePath: Option[Seq[Vec2]] = {
    in.site match {
      case "Girthline" => {
        val start = in.start.get  // this must be defined for Girthline
        val end   = in.end.get    // this must be defined for Girthline
        val coords = for {
          i <- start until end
          mesh = markerGrid.diceToTrimesh(i)
          meshDistance = mesh.distanceTo(Vec3(pointer.co(i)))
        } yield meshDistance.st
        Some(coords)
      }
      case _ => None
    }
  }
  
  //--------------------------------------------------------------------------
  // Grid average of the first invariant of the left Cauchy-Green
  // deformation tensor at each time sample (\bar{I_1}_{(\mu\mu0)}).
  private lazy val i1Bar: IndexedSeq[Double] = 
    for (i <- 0 until nSamples) yield 
      markerGrid.avgLCauchyGreenI1(refSample, i)
  
  //--------------------------------------------------------------------------
  // Maximum response sample (\mu_*).
  //
  // The maximum response sample is the first peak in the i1Bar values
  // following the poke sample.  If it happens that a peak never occurs
  // (monotonically increasing or decreasing i1Bar), then we issue a warning
  // to the console and use the last sample of the trial.  However, in the
  // test trials, this doesn't happen.
  private lazy val maxResponseSample: Int = {
    // finite-difference of successive i1Bar values
    val i1BarDot = for {
      (x0, x1) <- i1Bar zip (i1Bar.tail)
      delta = x1 - x0
    } yield delta
    // identify the first positive gradient after the reference sample
    val pgSample = i1BarDot.indexWhere(_ > 0, refSample)
    // first negative gradient after pgSample
    val ngSample = i1BarDot.indexWhere(_ < 0, pgSample)
    // hand the case where a peak doesn't exist (hopefully should never
    //  happen)
    val peakSample = if (ngSample == -1) {
      println("WARNING: Peak in i1Bar not found; setting maxResponseSample " +
          "to end for trial %s, site %s." format (idString, in.site))
      nSamples - 1
    } else {
      ngSample
    }
    peakSample
  }
    
  //--------------------------------------------------------------------------
  // Grid of I1 at the maximum response sample (peak).
  //
  // This is a grid (NOT interpolated) of the first invariant of the left
  // Cauchy-Green deformation tensor at the maximum response sample.
  private lazy val i1Grid_peak: Grid[Double] = 
    markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
  
  //--------------------------------------------------------------------------
  // Interpolated grid of I1 at the maximum response sample (peak).
  //
  // This is an INTERPOLATED grid of the first invariant of the left
  // Cauchy-Green deformation tensor at the maximum response sample.  The grid
  // is bi-cubically interpolated at 10x its original resolution.
  private lazy val i1Grid_interp_peak: Grid[Double] = {
    val iRows = i1Grid_peak.numRows * 10
    val iCols = i1Grid_peak.numCols * 10
    BicubicInterpGrid(i1Grid_peak).toGrid(iRows, iCols)
  }
    
  //--------------------------------------------------------------------------
  // Maximum I1 value at the maximum response.
  //
  // Here, we take a grid of I1 and interpolate it at 10x its original
  // resolution (using bi-cubic interpolation).  Then, we find the maximum
  // value of I1 from that grid.
  private lazy val i1_peak: Double = i1Grid_interp_peak.rowMajor.max
  
  //--------------------------------------------------------------------------
  // Location on B (st space) of the peak i1 value.
  //
  // This is found on a grid of I1 which is interpolated at 10x its original
  // resolution (using bi-cubic interpolation).
  private lazy val i1_peak_st: Vec2 = i1Grid_interp_peak.maxUV

  //--------------------------------------------------------------------------
  // Spatial location of the peak i1 value.
  //
  // This is found on a grid of I1 which is interpolated at 10x its original
  // resolution (using bi-cubic interpolation).
  private lazy val i1_peak_e: Vec3 = {
    val mesh = markerGrid.diceToTrimesh(maxResponseSample)
    val st = mesh.texCoordsToPoint(i1_peak_st)
    if (!st.isDefined) {
      println("Warning: (s,t) coordinates of the i1_peak were not " +
          "found in the mesh.  Setting point to (0,0).")
      Vec3.Zero
    } else {
      st.get
    }
  }
  
  // find the i1 value at the poke location at the maximum response
  private lazy val i1AtPokeLocation: Option[Double] = {
    if (pokeMeshDistance.isDefined) {
      val st = pokeMeshDistance.get.st
      val i1Grid = markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
      Some(i1Grid.interpUV(st.x, st.y)(
          () => Linearizable.doubleMultiplyableToLinearizable[Double]))
    } else {
      None
    }
  }
  
  // the distance from the poke location to the location of the maximum I1
  //  response
  private lazy val distanceFromPokeToMaxI1: Option[Double] = {
    if (pokeMeshDistance.isDefined) {
      Some((i1_peak_e - pokeMeshDistance.get.meshPoint).length)
    } else {
      None
    }
  }
  
  // compute the Biot strain tensor in 2D, at the maximum response sample,
  //  relative to the reference sample
  private lazy val biot2d: Grid[Mat2] = 
    markerGrid.biot2D(refSample, maxResponseSample)

  // find the minimum principal strain (compressive) with the largest magnitude
  //  at the maximum response sample.  here we interpolate a grid of biot2d
  //  at 10x its original resolution.
  private lazy val minPrinStrainAtMaxResponse: Double = {
    val biotGrid: Grid[Mat3] = markerGrid.biot(refSample, maxResponseSample)
    def minPS(m: Mat3): Double = m.eigSymm.map(_._1).min
    val newRows = biot2d.numRows * 10
    val newCols = biot2d.numCols * 10
    val minPSGrid = BicubicInterpGrid(biotGrid.map(minPS)).
        toGrid(newRows, newCols)
    minPSGrid.rowMajor.min
  }
    
    
  lazy val result: TrialResult = new TrialResult(
    in,
    idString,
    nSamples,
    fs,
    distance,
    refSample,
    pokeMeshDistance,
    strokePath,
    i1Bar,
    maxResponseSample,
    i1Grid_peak,
    i1_peak,
    i1_peak_st,
    i1_peak_e,
    i1AtPokeLocation,
    distanceFromPokeToMaxI1,
    biot2d,
    minPrinStrainAtMaxResponse
  )

}


case class TrialResult(
  in: TrialInput,
  idString: String,
  nSamples: Int,
  fs: Double,
  distance: Seq[MeshDistance],
  refSample: Int,
  pokeMeshDistance: Option[MeshDistance],
  strokePath: Option[Seq[Vec2]],
  i1Bar: IndexedSeq[Double],
  maxResponseSample: Int,
  i1Grid_peak: Grid[Double],
  i1_peak: Double,
  i1_peak_st: Vec2,
  i1_peak_e: Vec3,
  i1AtPokeLocation: Option[Double],
  distanceFromPokeToMaxI1: Option[Double],
  biot2d: Grid[Mat2],
  minPrinStrainAtMaxResponse: Double
)


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
