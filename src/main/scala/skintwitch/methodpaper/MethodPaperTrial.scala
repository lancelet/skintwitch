package skintwitch.methodpaper

import skintwitch.analysis.TrialInput

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

  // form unique identifier string for the trial
  val idString = "%s_trial%s" format (in.horse, in.trialNumber)
  
  
}