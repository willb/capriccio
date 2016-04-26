/*
 * sink.scala
 * 
 * author:  William Benton <willb@redhat.com>
 *
 * Copyright (c) 2010-2016 Red Hat, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.freevariable.capriccio.immutable

/**
 * On-line mean and variance estimates for a stream of Double values.
 * Supports combining estimates of multiple streams.  Uses Chan's technique
 * (later refined by Terriberry and Pébay).
 */
sealed case class Sink(val count: Long, val min: Double, val max: Double, val mean: Double, val m2: Double) {
  // TODO:  parameterize this over sample (at least), fractional (mean/variance), and integral (count) types
  
  // TODO:  use Pébay's formulae (supporting arbitrary moments) to make this code more generally useful

  @inline def put(sample: Double) = {
    val dev = (sample - mean)
    val newCount = count + 1
    val newMean = mean + (dev / (newCount))
    val newM2 = m2 + (dev * dev) * count / newCount
    
    this.copy(count=newCount, min=math.min(min, sample), max=math.max(max, sample), mean=newMean, m2=newM2)
  }
  
  def ++(other: Sink) = {
    (this, other) match {
      case (empty, _) if empty.count == 0L => other
      case (_, empty) if empty.count == 0L => this
      case _ => {
	val dev = other.mean - mean
	val newCount = this.count + other.count
	val newMean = (this.count * this.mean + other.count * other.mean) / newCount
	val newM2 = this.m2 + other.m2 + (dev * dev) * this.count * other.count / newCount
	this.copy(count=newCount, min=math.min(this.min, other.min), max=math.max(this.max, other.max), mean=newMean, m2=newM2)
      }
    }
  }

  val variance = m2 / count
  def stddev = math.sqrt(variance)
}

object Sink {
  def empty: Sink = Sink(0, Double.PositiveInfinity, Double.NegativeInfinity, 0.0d, 0.0d)
}
