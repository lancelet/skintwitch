package skintwitch.mesh

import skintwitch.Vec2
import skintwitch.Vec3
//import skintwitch.rman.V3

/** Triangular facet. 
 * 
 *  The outward-facing normal of the triangle is defined by the right-handed
 *  ordering of the vertices: a, b, c. */
trait Tri {
  
  /** Vertex of the triangle. */
  val a: Vec3
  /** Vertex of the triangle. */
  val b: Vec3
  /** Vertex of the triangle. */
  val c: Vec3
  /** Texture coordinates at `a`. */
  val ast: Vec2
  /** Texture coordinates at `b`. */
  val bst: Vec2
  /** Texture coordinates at `c`. */
  val cst: Vec2

  private lazy val abxac: Vec3 = (b - a) cross (c - a)
  private lazy val n: Vec3 = abxac.n
  
  /** Normal of the triangle. */
  lazy val normal: Vec3 = n
  
  /** Projects a point into the plane defined by the triangle.
   * 
   *  @param p point to project
   *  @return projected point */
  def projectInto(p: Vec3): Vec3 = {
    val ap = p - a
    a + ap - n * (ap dot n)
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
      p: Vec3,
      pAlreadyProjected: Boolean = false
  ): Vec3 = 
  {
    val pp = implicitly[Vec3](if (pAlreadyProjected) p else projectInto(p))
    val a1 = (((c - b) cross (pp - b)) dot n) * 0.5
    val a2 = (((a - c) cross (pp - c)) dot n) * 0.5
    val a3 = (((b - a) cross (pp - a)) dot n) * 0.5
    val aTot = a1 + a2 + a3
    val l1 = a1 / aTot
    val l2 = a2 / aTot
    val l3 = a3 / aTot
    Vec3(l1, l2, l3)
  }
  
  /** Computes the "signed distance" between a point and the triangle.
   *  
   *  The normal to the triangle is found via the right-hand rule.  The
   *  distance is positive if it lies on the outward-facing side of the
   *  triangle, and negative if it lies on the inward-facing side.
   */
  def signedDistanceTo(p: Vec3):
  (Double, Vec3) = {
    val (udist, pContact) = distanceTo(p)
    val sign = math.signum((p - pContact) dot normal)
    (sign * udist, pContact)
  }

  /** Computes the linearly-interpolated texture coordinates of a point in the 
   *  triangle.  The point should already have been projected to the plane of 
   *  the triangle for correct results.
   *  
   *  @param p point in the plane of the triangle
   *  @return texture coordinates `(s, t)` of the point */
  def texCoordsOfPoint(p: Vec3): Vec2 = {
    val bary = projectIntoBarycentric(p, true)
    val svec = Vec3(ast.x, bst.x, cst.x)
    val tvec = Vec3(ast.y, bst.y, cst.y)
    Vec2(bary dot svec, bary dot tvec)
  }
  
  /** Computes the shortest distance between a point and the triangle.
   *  
   *  @param p point for which to find the shortest distance
   *  @return shortest distance between the triangle and `p`, and the
   *    point at which the shortest distance was found */
  def distanceTo(p: Vec3): 
  (Double, Vec3) = {
    val pp = projectInto(p)
    val bary = projectIntoBarycentric(p, true)
    if (bary.x >= 0.0 && bary.x <= 1.0 &&
        bary.y >= 0.0 && bary.y <= 1.0 &&
        bary.z >= 0.0 && bary.z <= 1.0) { // inside the triangle
      ((pp - p).length, pp)
    } else if (bary.y < 0 && bary.z < 0) { // definitely closest to vertex a
      ((a - p).length, a)
    } else if (bary.x < 0 && bary.z < 0) { // definitely closest to vertex b
      ((b - p).length, b)
    } else if (bary.x < 0 && bary.y < 0) { // definitely closest to vertex c
      ((c - p).length, c)
    } else {
      // here, we may be closest to either an edge or a vertex, so find
      //  the closest edge and use the distanceToEdge() method
      val (ea, eb) = if (bary.z < 0) {
        (a, b)
      } else if (bary.y < 0) {
        (a, c)
      } else {
        assert(bary.x < 0)
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
  private def distanceToEdge(ea: Vec3,
                             eb: Vec3,
                             p: Vec3): 
  (Double, Vec3) = 
  {
    val eaeb = eb - ea
    val q = (eaeb dot (p - ea)) / eaeb.length2
    val qnorm = if (q < 0) 0 else if (q > 1) 1 else q
    val linePt = ea + eaeb * qnorm
    ((linePt - p).length, linePt)
  }
  
}

