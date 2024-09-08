package com.alchemist_al.sketches.theta_sketch

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._

class ShardThetaSketch(shardCount: Int) {
  import java.util.Map

  private val thetaSketches: Map[Int, ThetaSketchSimple] =
    new ConcurrentHashMap[Int, ThetaSketchSimple]()

  // Initialize sketches for each shard
  for (i <- 0 until shardCount) {
    thetaSketches.put(i, new ThetaSketchSimple())
  }

  // Function to hash the element into a shard
  def getShard(element: Any): Int = {
    val hash = element.hashCode()
    Math.abs(hash % shardCount)
  }

  // Add element to the correct shard
  def add(element: Any): Unit = {
    val shardIndex = getShard(element)
    val sketch = thetaSketches.get(shardIndex)
    sketch.add(element)
  }

  // Union all sketches from different shards to get the final sketch
  def union(): ThetaSketchSimple = {
    val resultSketch = new ThetaSketchSimple()
    thetaSketches.asScala.values.foreach { shardSketch =>
      resultSketch.union(shardSketch)
    }
    resultSketch
  }

  // Intersect all sketches from different shards
  def intersect(): ThetaSketchSimple = {
    val resultSketch = new ThetaSketchSimple()
    thetaSketches.asScala.values.foreach { shardSketch =>
      resultSketch.intersect(shardSketch)
    }
    resultSketch
  }

  // Estimate cardinality
  def estimateCardinality(): Double = {
    union().estimateCardinality()
  }
}

class ThetaSketchSimple {
  import java.util.{Map, HashMap}
  private val sketchMap: Map[Long, Long] = new HashMap()

  // Add element by hashing
  def add(element: Any): Unit = {
    val hash = element.hashCode().toLong
    sketchMap.put(hash, hash)
  }

  // Union with another ThetaSketch
  def union(other: ThetaSketchSimple): Unit = {
    other.sketchMap.forEach((key, value) => sketchMap.put(key, value))
  }

  // Intersect with another ThetaSketch
  def intersect(other: ThetaSketchSimple): Unit = {
    val intersection = new HashMap[Long, Long]()
    sketchMap.forEach { (key, value) =>
      if (other.sketchMap.containsKey(key)) {
        intersection.put(key, value)
      }
    }
    sketchMap.clear()
    sketchMap.putAll(intersection)
  }

  // Estimate cardinality (size of the sketch map)
  def estimateCardinality(): Double = {
    sketchMap.size().toDouble
  }
}

// Test the sharded Theta Sketch
object ThetaSketchTest extends App {
  val shardCount = 4
  val shardedThetaSketch = new ShardThetaSketch(shardCount)

  // Adding elements
  shardedThetaSketch.add("element1")
  shardedThetaSketch.add("element2")
  shardedThetaSketch.add("element3")
  shardedThetaSketch.add("element1") // Duplicate element

  // Estimate cardinality
  println(s"Estimated cardinality: ${shardedThetaSketch.estimateCardinality()}")

  // Intersect test (no intersection in this simple example)
  val intersectedSketch = shardedThetaSketch.intersect()
  println(
    s"Cardinality after intersection: ${intersectedSketch.estimateCardinality()}"
  )
}
