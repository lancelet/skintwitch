package skintwitch.mesh

import scala.collection.immutable._

import skintwitch.Vec2
import skintwitch.Vec3

class TriMesh(
  vertices: IndexedSeq[Vec3],
  faces: IndexedSeq[(Int, Int, Int)],
  texCoords: Option[IndexedSeq[Vec2]] = None)
{
  // check face indices are within the allowed range and unique within a
  //  triangle
  require(faces.forall(f =>
    f._1 >= 0 && f._1 < vertices.length &&
    f._2 >= 0 && f._2 < vertices.length &&
    f._3 >= 0 && f._3 < vertices.length &&
    f._1 != f._2 &&
    f._1 != f._3 &&
    f._2 != f._3
  ))
  // check that, if texture coordinates are provided, there is one texture
  //  coordinate per vertex
  if (texCoords.isDefined) {
    require(texCoords.get.length == vertices.length)
  }
  
  /** A Tri belonging to a TriMesh. */
  private case class TriMeshTri(
    a: Vec3,
    b: Vec3,
    c: Vec3,
    ast: Vec2,
    bst: Vec2,
    cst: Vec2) extends Tri
  
  /** IndexedSeq of tris within this TriMesh. */
  val tris: IndexedSeq[Tri] = new IndexedSeq[Tri] {
    val length = faces.length
    def apply(index: Int): Tri = {
      val face = faces(index)
      val st = if (texCoords.isDefined) {
        val txc = texCoords.get
        (txc(face._1), txc(face._2), txc(face._3))
      } else {
        (Vec2.Zero, Vec2.Zero, Vec2.Zero)
      }
      TriMeshTri(vertices(face._1), vertices(face._2), vertices(face._3),
                 st._1, st._2, st._3)
    }
  }

  /** Shortest distance between this mesh and a given point.
   * 
   *  This implementation cycles over all triangles in the mesh (in
   *  parallel), and then sorts distances to individual triangles to find
   *  the shortest distance.  There may be a more optimised method which
   *  could take into account some approximate metric first and then perform
   *  precise distance testing as a second step.
   *
   *  @param p the point for which to find the shortest distance
   *  @return the shortest distance from the mesh to point `p`, the
   *    point on the mesh at which the shortest distance occurs, and the
   *    texture coordinates of the point */
  def distanceTo(p: Vec3): 
  (Double, Vec3, Vec2) = {
    val closestPoints = for {
      tri <- tris.par
      (dist, xPoint) = tri.distanceTo(p)
    } yield (dist, xPoint, tri)
    val (dist, xPoint, tri) = closestPoints.seq.sortBy(_._1).head
    (dist, xPoint, tri.texCoordsOfPoint(xPoint))
  }
  
  /** Shortest signed distance between this mesh and a given point.
   * 
   *  The signed distance is positive if a point lies on the outside of the
   *  mesh, and negative if the point lies on the inside of the mesh (as
   *  determined by right-hand surface normals).  The method finds the
   *  absolute shortest distance between the point and the grid, but the
   *  distance is signed.
   *  
   *  @param p the point for which to find the signed distance
   *  @return the shortest signed distance from the mesh to point `p`, the
   *    point on the mesh at which the shortest signed distance occurs, and
   *    the texture coordinates of the point */
  def signedDistanceTo(p: Vec3):
  (Double, Vec3, Vec2) = {
    val closestPoints = for {
      tri <- tris.par
      (dist, xPoint) = tri.signedDistanceTo(p)
    } yield (dist, xPoint, tri)
    val (dist, xPoint, tri) = closestPoints.seq.sortBy(x => math.abs(x._1)).
      head
    (dist, xPoint, tri.texCoordsOfPoint(xPoint))
  }

}
