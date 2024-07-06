package com.alchemist_al.sketches.theta_sketch

import scala.collection.mutable

class ThetaSketch {

  private val sketchSet: mutable.Set[Long] = mutable.Set()

  def add(element: Any): Unit =
    sketchSet += element.hashCode.toLong

  def estimate: Long =
    sketchSet.size.toLong

  def union(other: ThetaSketch): ThetaSketch =
    val unionSketch = new ThetaSketch
    unionSketch.sketchSet ++= this.sketchSet
    unionSketch.sketchSet ++= other.sketchSet
    unionSketch

  def intersect(other: ThetaSketch): ThetaSketch =
    val intersectionSketch = new ThetaSketch
    for (value <- this.sketchSet)
      if (other.sketchSet.contains(value))
        intersectionSketch.sketchSet.add(value)
    intersectionSketch

}
