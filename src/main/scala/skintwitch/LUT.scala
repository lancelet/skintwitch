package skintwitch

import java.awt.Color

/** A look-up-table.
  *
  * A LUT maps a Double, in the range [0,1], to a Color. */
trait LUT extends Function1[Double, Color]