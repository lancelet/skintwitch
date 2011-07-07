package skintwitch.mesh

import skintwitch.rman.V3

/** Triangular facet. 
 * 
 *  The outward-facing normal of the triangle is defined by the right-handed
 *  ordering of the vertices: a, b, c. */
trait Tri {
  import Tri._
  
  /** Vertex of the triangle. */
  val a: (Double, Double, Double)
  /** Vertex of the triangle. */
  val b: (Double, Double, Double)
  /** Vertex of the triangle. */
  val c: (Double, Double, Double)

  private lazy val abxac: V3 = (b - a) x (c - a)
  private lazy val n: V3 = abxac.normalized
  
  /** Normal of the triangle. */
  lazy val normal: (Double, Double, Double) = (n.e0, n.e1, n.e2)
  
  /** Projects a point into the plane defined by the triangle.
   * 
   *  @param p point to project
   *  @return projected point */
  def projectInto(p: (Double, Double, Double)): (Double, Double, Double) = {
    val ap = p - a
    val pp = a + ap - n * (ap dot n)
    (pp.e0, pp.e1, pp.e2)
  }
  
  /** Projects a point into the plane defined by the triangle and computes
   *  its barycentric coordinates.
   *  
   *  The barycentric coordinates are ordered so that they reference the
   *  (a, b, c) vertices respectively.
   *  
   *  @param p point to project
   *  @param pAlreadyProjected true if `p` has already been projected into
   *    the plane of the triangle
   *  @return barycentric coordinates of `p` */
  def projectIntoBarycentric(
      p: (Double, Double, Double),
      pAlreadyProjected: Boolean = false
  ): 
    (Double, Double, Double) = 
  {
    val pp = implicitly[V3](if (pAlreadyProjected) p else projectInto(p))
    val a1 = (((c - b) x (pp - b)) dot n) * 0.5
    val a2 = (((a - c) x (pp - c)) dot n) * 0.5
    val a3 = (((b - a) x (pp - a)) dot n) * 0.5
    val aTot = a1 + a2 + a3
    val l1 = a1 / aTot
    val l2 = a2 / aTot
    val l3 = a3 / aTot
    (l1, l2, l3)
  }
  
  /** Computes the "signed distance" between a point and the triangle.
   *  
   *  The normal to the triangle is found via the right-hand rule.  The
   *  distance is positive if it lies on the outward-facing side of the
   *  triangle, and negative if it lies on the inward-facing side.
   */
  def signedDistanceTo(p: (Double, Double, Double)):
  (Double, (Double, Double, Double)) = {
    val (udist, pContact) = distanceTo(p)
    val sign = math.signum((p - pContact) dot normal)
    (sign * udist, pContact)
  }
  
  /** Computes the shortest distance between a point and the triangle.
   *  
   *  @param p point for which to find the shortest distance
   *  @return shortest distance between the triangle and `p`, and the
   *    point at which the shortest distance was found */
  def distanceTo(p: (Double, Double, Double)): 
  (Double, (Double, Double, Double)) = {
    val pp = projectInto(p)
    val bary = projectIntoBarycentric(p, true)
    if (bary._1 >= 0.0 && bary._1 <= 1.0 &&
        bary._2 >= 0.0 && bary._2 <= 1.0 &&
        bary._3 >= 0.0 && bary._3 <= 1.0) { // inside the triangle
      ((pp - p).length, pp)
    } else if (bary._2 < 0 && bary._3 < 0) { // definitely closest to vertex a
      ((a - p).length, a)
    } else if (bary._1 < 0 && bary._3 < 0) { // definitely closest to vertex b
      ((b - p).length, b)
    } else if (bary._1 < 0 && bary._2 < 0) { // definitely closest to vertex c
      ((c - p).length, c)
    } else {
      // here, we may be closest to either an edge or a vertex, so find
      //  the closest edge and use the distanceToEdge() method
      val (ea, eb) = if (bary._3 < 0) {
        (a, b)
      } else if (bary._2 < 0) {
        (a, c)
      } else {
        assert(bary._1 < 0)
        (c, b)
      }
      distanceToEdge(ea, eb, p)
    }
  }
  
  /** Finds the shortest distance between a point and an edge.
   * 
   *  @param ea edge vertex a
   *  @param eb edge vertex b
   *  @param p point for which to find the distance
   *  @return shortest distance between `ea -> eb` and `p`, and the
   *    point on the edge at which the shortest distance occurs */
  private def distanceToEdge(ea: (Double, Double, Double),
                             eb: (Double, Double, Double),
                             p: (Double, Double, Double)): 
  (Double, (Double, Double, Double)) = 
  {
    val eaeb = eb - ea
    val q = (eaeb dot (p - ea)) / eaeb.length2
    val qnorm = if (q < 0) 0 else if (q > 1) 1 else q
    val linePt = ea + eaeb * qnorm
    ((linePt - p).length, linePt)
  }
  
}

object Tri {
  implicit def tupleToV3(x: (Double, Double, Double)): V3 = 
    V3(x._1, x._2, x._3)
  implicit def V3ToTuple(v: V3): (Double, Double, Double) = (v.e0, v.e1, v.e2)
}