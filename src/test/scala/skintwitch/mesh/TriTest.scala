package skintwitch.mesh

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TriTest extends FunSuite with ShouldMatchers {

  test("normal") {
    val tri = new Tri {
      val a = (0.0, 0.0, 0.0)
      val b = (0.0, 0.0, 1.0)
      val c = (1.0, 0.0, 0.0)
    }
    val n = tri.normal
    n._1 should be (0.0 plusOrMinus 1e-10)
    n._2 should be (1.0 plusOrMinus 1e-10)
    n._3 should be (0.0 plusOrMinus 1e-10)
  }
  
  test("projectInto") {
    val tri = new Tri {
      val a = (0.0, 0.0, 0.0)
      val b = (0.0, 0.0, 1.0)
      val c = (1.0, 0.0, 0.0)      
    }
    val p = tri.projectInto((0.5, 50.0, 0.3))
    p._1 should be (0.5 plusOrMinus 1e-10)
    p._2 should be (0.0 plusOrMinus 1e-10)
    p._3 should be (0.3 plusOrMinus 1e-10)
  }

  test("projectIntoBarycentric") {
    val tri = new Tri {
      val a = (0.0, 0.0, 0.0)
      val b = (0.0, 0.0, 1.0)
      val c = (1.0, 0.0, 0.0)      
    }
    val b = tri.projectIntoBarycentric((0.0, 5.0, 0.0))
    b._1 should be (1.0 plusOrMinus 1e-10)
    b._2 should be (0.0 plusOrMinus 1e-10)
    b._3 should be (0.0 plusOrMinus 1e-10)
    val c = tri.projectIntoBarycentric((0.5, 5.0, 0.5))
    c._1 should be (0.0 plusOrMinus 1e-10)
    c._2 should be (0.5 plusOrMinus 1e-10)
    c._3 should be (0.5 plusOrMinus 1e-10)
    val d = tri.projectIntoBarycentric((1.0, -10.0, 1.0))
    d._1 should be (-1.0 plusOrMinus 1e-10)
    d._2 should be (1.0 plusOrMinus 1e-10)
    d._3 should be (1.0 plusOrMinus 1e-10)
  }
  
  test("distanceTo") {
    val tri = new Tri {
      val a = (-1.0, -1.0, 0.0)
      val b = ( 1.0, -1.0, 0.0)
      val c = (-1.0,  1.0, 0.0)
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
  
}
