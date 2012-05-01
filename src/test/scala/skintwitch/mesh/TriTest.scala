package skintwitch.mesh

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import skintwitch.Vec2
import skintwitch.Vec3

class TriTest extends FunSuite with ShouldMatchers {

  test("normal") {
    val tri = new Tri {
      val a = Vec3(0.0, 0.0, 0.0)
      val b = Vec3(0.0, 0.0, 1.0)
      val c = Vec3(1.0, 0.0, 0.0)
      val ast = Vec2.Zero
      val bst = Vec2.Zero
      val cst = Vec2.Zero
    }
    val n = tri.normal
    n.x should be (0.0 plusOrMinus 1e-10)
    n.y should be (1.0 plusOrMinus 1e-10)
    n.z should be (0.0 plusOrMinus 1e-10)
  }
  
  test("projectInto") {
    val tri = new Tri {
      val a = Vec3(0.0, 0.0, 0.0)
      val b = Vec3(0.0, 0.0, 1.0)
      val c = Vec3(1.0, 0.0, 0.0)      
      val ast = Vec2.Zero
      val bst = Vec2.Zero
      val cst = Vec2.Zero
    }
    val p = tri.projectInto((0.5, 50.0, 0.3))
    p.x should be (0.5 plusOrMinus 1e-10)
    p.y should be (0.0 plusOrMinus 1e-10)
    p.z should be (0.3 plusOrMinus 1e-10)
  }

  test("projectIntoBarycentric") {
    val tri = new Tri {
      val a = Vec3(0.0, 0.0, 0.0)
      val b = Vec3(0.0, 0.0, 1.0)
      val c = Vec3(1.0, 0.0, 0.0)      
      val ast = Vec2.Zero
      val bst = Vec2.Zero
      val cst = Vec2.Zero
    }
    val b = tri.projectIntoBarycentric((0.0, 5.0, 0.0))
    b.x should be (1.0 plusOrMinus 1e-10)
    b.y should be (0.0 plusOrMinus 1e-10)
    b.z should be (0.0 plusOrMinus 1e-10)
    val c = tri.projectIntoBarycentric((0.5, 5.0, 0.5))
    c.x should be (0.0 plusOrMinus 1e-10)
    c.y should be (0.5 plusOrMinus 1e-10)
    c.z should be (0.5 plusOrMinus 1e-10)
    val d = tri.projectIntoBarycentric((1.0, -10.0, 1.0))
    d.x should be (-1.0 plusOrMinus 1e-10)
    d.y should be (1.0 plusOrMinus 1e-10)
    d.z should be (1.0 plusOrMinus 1e-10)
  }
  
  test("distanceTo") {
    val tri = new Tri {
      val a = Vec3(-1.0, -1.0, 0.0)
      val b = Vec3( 1.0, -1.0, 0.0)
      val c = Vec3(-1.0,  1.0, 0.0)
      val ast = Vec2.Zero
      val bst = Vec2.Zero
      val cst = Vec2.Zero      
    }
    val (a, pta) = tri.distanceTo((-0.453, -0.472, 1.0))
    val (b, ptb) = tri.distanceTo((0.0, -2.0, 1.0))
    val (c, ptc) = tri.distanceTo((1.707, -1.707, 1.0))
    val (d, ptd) = tri.distanceTo((1.892, -1.452, 1.0))
    a should be (1.0 plusOrMinus 1e-10)
    b should be (1.41 plusOrMinus 1e-2)
    c should be (1.41 plusOrMinus 1e-2)
    d should be (1.41 plusOrMinus 1e-2)
  }

  test("signedDistanceTo") {
    val tri = new Tri {
      val a = Vec3(-1.0, -1.0, 0.0)
      val b = Vec3( 1.0, -1.0, 0.0)
      val c = Vec3(-1.0,  1.0, 0.0)
      val ast = Vec2.Zero
      val bst = Vec2.Zero
      val cst = Vec2.Zero      
    }
    val (a, pta) = tri.signedDistanceTo((-0.453, -0.472, 1.0))
    val (b, ptb) = tri.signedDistanceTo((0.0, -2.0, 1.0))
    val (c, ptc) = tri.signedDistanceTo((1.707, -1.707, -1.0))
    val (d, ptd) = tri.signedDistanceTo((1.892, -1.452, 1.0))
    a should be (1.0 plusOrMinus 1e-10)
    b should be (1.41 plusOrMinus 1e-2)
    c should be (-1.41 plusOrMinus 1e-2)
    d should be (1.41 plusOrMinus 1e-2)
  }
  
  test("texCoordsOfPoint") {
    val tri = new Tri {
      val a = Vec3(0.0, 0.0, 0.0)
      val b = Vec3(1.0, 0.0, 0.0)
      val c = Vec3(0.0, 1.0, 0.0)
      val ast = Vec2(0.0, 0.0)
      val bst = Vec2(0.0, 1.0)
      val cst = Vec2(1.0, 0.0)
    }
    val st = tri.texCoordsOfPoint((0.5, 0.5, 0.0))
    st.x should be (0.5 plusOrMinus 1e-10)
    st.y should be (0.5 plusOrMinus 1e-10)
  }
  
}
