package skintwitch.mesh

import skintwitch.{Vec2, Vec3}

/** Distance information for a query between a TriMesh and a point.
  * 
  * @param point point in 3D space which was queried
  * @param meshPoint the closest point on the mesh to the original queried
  *   point
  * @param st (s,t) coordinates of the mesh at which meshPoint is located
  * @param pointIsOutside true if the point lies "outside" the mesh, meaning
  *   that it lies along the direction of the positive normal to the closest
  *   triangle of the mesh.
  */
final case class MeshDistance(point: Vec3, meshPoint: Vec3, st: Vec2,
    pointIsOutside: Boolean) {
  
  /** Distance between point and meshPoint. */
  lazy val distance: Double = {
    val sign = if (pointIsOutside) 1.0 else -1.0
    (point - meshPoint).length * sign
  }
  
  /** Checks whether the provided st coordinates are within the grid.
    *
    * @param tolerance amount by which to shrink the edges of the grid
    *   (shrinks the edges of the st coordinate space).
    * 
    * @return true if each of the st coordinates are in the range
    *   [tolerance, 1-tolerance] */
  def stInGrid(tolerance: Double = 0.01): Boolean = {
    val minm = tolerance
    val maxm = 1 - tolerance
    (st.x > minm) && (st.x < maxm) && (st.y > minm) && (st.y < maxm)
  }
  
}
