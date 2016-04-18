package com.freevariable.capriccio

sealed abstract class Sink {
  val count: Long
  val min: Double
  val max: Double
  val mean: Double
  val variance: Double
  val sumX2: Double
  def put(sample: Double): Sink
}

case object EmptySink extends Sink {
  val count = 0l
  val min = Double.PositiveInfinity
  val max = Double.NegativeInfinity
  val mean = 0.0d
  val variance = 0.0d
  val sumX2 = 0.0d
  def put(sample: Double) = this
}

sealed case class UnemptySink(val count: Long, val min: Double, val max: Double, val mean: Double, val variance: Double, val sumX2: Double) extends Sink {
  def put(sample: Double) = this

  private def update(sample: Double) = {
    
  }
}
