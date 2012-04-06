package skintwitch

import java.awt.Color

/** Look-up table which maps to the classic "spectrum" LUT. */
object SpectrumLUT extends LUT {
  def apply(x: Double): Color = {
    require(x >= 0.0 && x <= 1.0)
    val (r, g, b) = if (x < 0.5) {
      (0.0, 2 * x, 1 - 2 * x)
    } else {
      (2 * x - 1, 2 - 2 * x, 0.0)
    }
    new Color(r.toFloat, g.toFloat, b.toFloat)
  }
}
