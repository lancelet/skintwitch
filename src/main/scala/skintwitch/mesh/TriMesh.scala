package skintwitch.mesh

import scala.collection.immutable._

class TriMesh(
  vertices: IndexedSeq[(Double, Double, Double)], 
  faces: IndexedSeq[(Int, Int, Int)])
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
  
  /** A Tri belonging to a TriMesh. */
  private case class TriMeshTri(
    a: (Double, Double, Double),
    b: (Double, Double, Double),
    c: (Double, Double, Double)) extends Tri
  
  /** IndexedSeq of tris within this TriMesh. */
  val tris: IndexedSeq[Tri] = new IndexedSeq[Tri] {
    val length = faces.length
    def apply(index: Int): Tri = {
      val face = faces(index)
      TriMeshTri(vertices(face._1), vertices(face._2), vertices(face._3))
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
   *  @return the shortest distance from the mesh to point `p`, and the
   *    point on the mesh at which the shortest distance occurs */
  def distanceTo(p: (Double, Double, Double)): 
    (Double, (Double, Double, Double)) =
      tris.par.map(_.distanceTo(p)).seq.sortBy(_._1).head

}