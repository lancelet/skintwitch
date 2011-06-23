package skintwitch.rman

import math.abs
import scala.collection.immutable._
import ri.BoundBox

case class BBox(minX: Double, maxX: Double, 
                minY: Double, maxY: Double,
                minZ: Double, maxZ: Double)
{
  def includePt(v: V3) = BBox(
    if (v.e0 < minX) v.e0 else minX,
    if (v.e0 > maxX) v.e0 else maxX,
    if (v.e1 < minY) v.e1 else minY,
    if (v.e1 > maxY) v.e1 else maxY,
    if (v.e2 < minZ) v.e2 else minZ,
    if (v.e2 > maxZ) v.e2 else maxZ
  )
  def +(v: V3) = includePt(v) 
  def growBy(c: Double) = {
    val a = abs(c)
    BBox(
      minX - a, maxX + a,
      minY - a, maxY + a,
      minZ - a, maxZ + a
    )
  }
  lazy val rman = BoundBox(minX, maxX, minY, maxY, minZ, maxZ)
}

object BBox {
  def apply(v: V3): BBox = BBox(v.e0, v.e0, v.e1, v.e1, v.e2, v.e2)
  def apply(v: Seq[V3]): BBox = v.tail.foldLeft(BBox(v.head))(_ + _)
}