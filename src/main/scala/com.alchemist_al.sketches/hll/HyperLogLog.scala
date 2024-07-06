package com.alchemist_al.sketches.hll

import scala.collection.mutable.ArrayBuffer

class HyperLogLog(b: Int) {

  private val m: Int = 1 << b // Number of registers
  private val registers: ArrayBuffer[Int] = ArrayBuffer.fill(m)(0)
  private val alphaMM: Double = computeAlphaMM(m)

  // Method to compute alphaMM (a constant for bias correction)
  private def computeAlphaMM(m: Int): Double =
    m match {
      case 16 => 0.673 * m * m
      case 32 => 0.697 * m * m
      case 64 => 0.709 * m * m
      case _  => (0.7213 / (1 + 1.079 / m)) * m * m
    }

  def add(element: Any): Unit = {
    // Hash element to get a 64-bit hash value (simulate with hashCode() for simplicity)
    val hash = element.hashCode()

    // Extract register index (using first b bits of hashValue)
    val index = hash >>> (Integer.SIZE - b) // hashValue % m

    // Determine position of the leftmost 1-bit (position of the most significant bit set to 1)
    val w = (hash % math.pow(2, Integer.SIZE - b)).toInt | (1 << (b - 1))
    val rank = Integer.numberOfLeadingZeros(w) + 1

    // Update the register with the maximum rank encountered so far
    registers(index) = registers(index).max(rank)
  }

  def estimate: Double = {
    val Z = 1.0 / registers.map(r => 1.0 / (1 << r)).sum
    val rawEstimate = alphaMM * Z
    correctBias(rawEstimate)
  }

  // Method to correct the bias of the raw estimate
  private def correctBias(estimate: Double): Double = {
    if (estimate <= 2.5 * m) {
      val V = registers.count(_ == 0)
      if (V > 0) m * math.log(m.toDouble / V) else estimate
    } else if (estimate > (1 << 30) / 30.0) {
      -(1 << 31) * math.log(1.0 - estimate / (1 << 31))
    } else {
      estimate
    }
  }

  // Method to merge another HyperLogLog into this one
  def union(other: HyperLogLog): Unit = {
    if (this.m != other.m)
      throw new IllegalArgumentException(
        "Cannot union HyperLogLogs with different sizes"
      )
    for (i <- registers.indices) {
      registers(i) = registers(i).max(other.registers(i))
    }
  }

}
