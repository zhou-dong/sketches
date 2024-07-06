package com.alchemist_al.sketches.theta_sketch

class ThetaSketchTest extends munit.FunSuite {

  test("theta sketch") {
    val sketch1 = new ThetaSketch
    val sketch2 = new ThetaSketch

    sketch1.add("java")
    sketch1.add("scala")
    sketch1.add("goland")
    sketch1.add("rust")
    sketch1.add("python")
    sketch1.add("haskell")
    sketch1.add("clojure")

    sketch2.add("java")
    sketch2.add("scala")
    sketch2.add("javascript")
    sketch2.add("typescript")

    assertEquals(sketch1.estimate, 7L)
    assertEquals(sketch2.estimate, 4L)

    val union = sketch1.union(sketch2)
    assertEquals(union.estimate, 9L)

    val intersection = sketch1.intersect(sketch2)
    assertEquals(intersection.estimate, 2L)
  }

}
