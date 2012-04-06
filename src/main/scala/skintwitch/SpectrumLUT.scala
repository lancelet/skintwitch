package skintwitch

import java.awt.Color

/** Look-up table which maps to the classic "spectrum" LUT. */
object SpectrumLUT extends LUT {
  def apply(x: Double): Color = {
    if (x < 0.0 || x > 1.0) {
      new Color(0.5f, 0.5f, 0.5f)
    } else {
      val b = if (x <= 0.25) 1
              else if (x >= 0.50) 0
              else -4 * x + 2
      val g = if (x >= 0.25 && x <= 0.75) 1
              else if (x < 0.25) 4 * x
              else -4 * x + 4
      val r = if (x <= 0.5) 0
              else if (x >= 0.75) 1
              else 4 * x - 2
      new Color(r.toFloat, g.toFloat, b.toFloat)
    }
  }
}
