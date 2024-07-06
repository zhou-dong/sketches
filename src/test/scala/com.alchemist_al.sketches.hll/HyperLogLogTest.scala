package com.alchemist_al.sketches.hll

class HyperLogLogTest extends munit.FunSuite {

  test("HyperLogLog") {

    val registers = 14 // Using 2^14 registers
    val hll1 = new HyperLogLog(registers)
    val hll2 = new HyperLogLog(registers)

    hll1.add("java")
    hll1.add("scala")
    hll1.add("python")
    hll1.add("rust")
    hll1.add("haskell")
    hll1.add("clojure")

    hll2.add("java")
    hll2.add("scala")
    hll2.add("golang")
    hll2.add("javascript")
    hll2.add("typescript")

    assertEquals(hll1.estimate.toInt, 6)
    assertEquals(hll2.estimate.toInt, 5)

    hll1.union(hll2)
    assertEquals(hll1.estimate.toInt, 9)
  }

}
