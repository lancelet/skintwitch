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
  /*i1cutoffFreq: Double = 5.0,*/
  threshold: Double = 12.0,
  backoffTime: Double = 0.0
) {

  import Trial._
  
  // a unique identifier string for this trial
  private val idString = "%s_trial%s" format (in.horse, in.trialNumber)
  
  // load and filter markers from the main trial
  private lazy val markers: Seq[Marker] = 
    loadMarkers(in.inputFile).map(_.butter2(cutoffFreq))
  
  // create the marker grid
  private lazy val markerGrid: MarkerGrid = MarkerGrid.fromCRMarkers(markers)

  // set number of samples in the trial and the sampling frequency (just
  //  fetch them from the first marker for the grid; they have to be the
  //  same for all markers)
  private lazy val nSamples: Int = markerGrid(0, 0).co.length
  private lazy val fs: Double = {
    val fs0 = markerGrid(0, 0).fs
    assert(markers.forall(m => m.co.length == nSamples && m.fs == fs0))
    fs0
  }
  
  // create the virtual marker for the pointer tip
  private lazy val pointer: Marker = 
    createPointerTipMarker(in.pointerFile, markers)

  // distanceAnnotated is the distance from the pointer tip to the grid,
  //  and a second tuple element indicating whether the pointer at the
  //  computed distance lies "within" the grid
  private lazy val distanceAnnotated: Seq[(Double, Boolean)] = {
    def isWithinGrid(st: Vec2): Boolean = {
      val minm = 0.01
      val maxm = 1 - minm
      (st.x > minm) && (st.x < maxm) && (st.y > minm) && (st.y < maxm)
    }
    for {
      i <- 0 until nSamples
      mesh = markerGrid.diceToTrimesh(i)
      (distance, xPoint, st) = mesh.signedDistanceTo(Vec3(pointer.co(i)))
      inGrid = isWithinGrid(st)
    } yield (distance, inGrid)
  }
  
  // the reference sample (just before the poke occurs).  for Control trials,
  //  the reference sample is 0, while for Girthline trials, the reference
  //  sample is specified by `start`.
  private lazy val refSample: Int = refSampleOverride.getOrElse {
    val cRefSample = if (in.site == "Control") {
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
    assert(cRefSample >= 0 && cRefSample < nSamples)
    cRefSample
  }

  // find the poke location in st parametric coordinates.  there is no
  //  poke location for control trials and girthline trials
  private lazy val pokeLocation: Option[Vec2] = {
    if (in.site == "Control" || in.site == "Girthline") {
      None
    } else {
      val mesh = markerGrid.diceToTrimesh(refSample)
      val (distance, xPoint, st) = 
        mesh.signedDistanceTo(Vec3(pointer.co(refSample)))
      Some(st)
    }
  }
  
  // find the poke location (row, col) in grid coordinates.  there is no poke 
  //  location for control trials and girthline trials.
  private lazy val pokeGridLocation: Option[Vec2] = {
    if (pokeLocation.isDefined) {
      val st = pokeLocation.get
      Some(
        Vec2(st.y * (markerGrid.numRows - 1), st.x * (markerGrid.numCols - 1)))
    } else {
      None
    }
  }
  
  // find the poke location in spatial (3D) coordinates.  there is no poke
  //  location for control trials and girthline trials
  private lazy val pokeSpatialLocation: Option[Vec3] = {
    if (in.site == "Control" || in.site == "Girthline") {
      None
    } else {
      val mesh = markerGrid.diceToTrimesh(refSample)
      val (distance, xPoint, st) = 
        mesh.signedDistanceTo(Vec3(pointer.co(refSample)))
      Some(xPoint)
    }    
  }
  
  // find the stroke path for a Girthline trial.  the path is a set of st
  //  parametric coordinates describing the path of the tip marker during the
  //  period it is in contact with the skin
  private lazy val strokePath: Option[Seq[Vec2]] = {
    if (in.site == "Girthline") {
      val start = in.start.get  // this must be defined for Girthline
      val end   = in.end.get    // this must be defined for Girthline
      val coords = for {
        i <- start until end
        mesh = markerGrid.diceToTrimesh(i)
        (distance, xPoint, st) = mesh.signedDistanceTo(Vec3(pointer.co(i)))
      } yield st
      Some(coords)
    } else {
      None
    }
  }
  
  // compute the grid average of the first invariant of the left Cauchy-Green 
  //  Deformation tensor at each time sample.  this is computed over all
  //  samples, but only the samples after the poke are relevant.
  private lazy val i1: IndexedSeq[Double] = for {
    i <- 0 until nSamples
  } yield markerGrid.avgLCauchyGreenI1(refSample, i)
  
  // perform some extra filtering on i1, for the purpose of finding its
  //  initial peak value
  /*
  val i1Filt: IndexedSeq[Double] = {
    val sos = Butter.butterSOSEven(2, i1cutoffFreq / (fs / 2)).head
    val b = IndexedSeq(sos.b0, sos.b1, sos.b2)
    val a = IndexedSeq(   1.0, sos.a1, sos.a2)
    FiltFilt.filtfilt(b, a, i1)
  }*/
  private lazy val i1Filt: IndexedSeq[Double] = i1  // disable extra filtering
  
  // compute the maximum response sample.  this is the first peak in the
  //  i1 values following the poke
  private lazy val maxResponseSample: Int = {
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
    val mrs = if (maxCandidate == -1) {
      println("WARNING: Peak not found; setting maxResponseSample to end " +
      		"for trial %s, site %s." format(idString, in.site))
      println("         (This may not be a problem for Girthline trials.)")
      nSamples - 1
    } else {
      maxCandidate
    }
    assert(mrs > refSample && mrs < nSamples) 
    mrs
  }
  
  // find the peak I1 value at the maximum response sample
  private lazy val peakI1AtMaximumResponse: Double = {
    val i1Grid = markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
    i1Grid.rowMajor.max
  }
  
  // find the marker (row, col) with the peak i1 value
  private lazy val peakI1GridLocation: Vec2 = {
    // first form a grid of i1 values at the maximum response, and find
    //  the maximum marker
    val i1Grid = markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
    val (rowMax, colMax) = Averaging.maxCoords(i1Grid)
    Vec2(rowMax, colMax)
  }
  
  // find the spatial location of the marker with the peak i1 value
  private lazy val peakI1SpatialLocation: Vec3 = {
    val rowMax = peakI1GridLocation.x
    val colMax = peakI1GridLocation.y
    // now go back to the marker grid, and returns the coordinates of that
    //  marker at the maximum response
    Vec3(markerGrid(rowMax.toInt, colMax.toInt).co(maxResponseSample))
  }

  // find the i1 value at the poke location at the maximum response
  private lazy val i1AtPokeLocation: Option[Double] = {
    if (pokeLocation.isDefined) {
      val st = pokeLocation.get
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
    if (pokeSpatialLocation.isDefined) {
      Some((peakI1SpatialLocation - pokeSpatialLocation.get).length)
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
    
  // compute the I1 grid for this trial
  private lazy val i1Grid: Grid[Double] = 
    markerGrid.lCauchyGreenI1(refSample, maxResponseSample)
    
  lazy val result: TrialResult = new TrialResult(
    in,
    idString,
    nSamples,
    fs,
    distanceAnnotated,
    refSample,
    pokeLocation,
    pokeGridLocation,
    pokeSpatialLocation,
    strokePath,
    i1,
    maxResponseSample,
    peakI1AtMaximumResponse,
    peakI1GridLocation,
    peakI1SpatialLocation,
    i1AtPokeLocation,
    distanceFromPokeToMaxI1,
    biot2d,
    minPrinStrainAtMaxResponse,
    i1Grid
  )

}


case class TrialResult(
  in: TrialInput,
  idString: String,
  nSamples: Int,
  fs: Double,
  distanceAnnotated: Seq[(Double, Boolean)],
  refSample: Int,
  pokeLocation: Option[Vec2],
  pokeGridLocation: Option[Vec2],
  pokeSpatialLocation: Option[Vec3],
  strokePath: Option[Seq[Vec2]],
  i1: IndexedSeq[Double],
  maxResponseSample: Int,
  peakI1AtMaximumResponse: Double,
  peakI1GridLocation: Vec2,
  peakI1SpatialLocation: Vec3,
  i1AtPokeLocation: Option[Double],
  distanceFromPokeToMaxI1: Option[Double],
  biot2d: Grid[Mat2],
  minPrinStrainAtMaxResponse: Double,
  i1Grid: Grid[Double]
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
