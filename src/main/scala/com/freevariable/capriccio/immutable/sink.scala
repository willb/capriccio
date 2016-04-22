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
 * Uses the technique from <a href="http://dl.acm.org/citation.cfm?id=359153">"Updating mean and variance estimates: an improved method"</a>, by D. H. D. West (1979).
*/
sealed case class Sink(val count: Long, val min: Double, val max: Double, val mean: Double, val variance: Double, val sumX2: Double) {
  // TODO:  parameterize this over sample (at least), fractional (mean/variance), and integral (count) types
  
  // TODO:  rework to use Pébay's algorithm (support arbitrary moments, support combining stream estimates) to make this code more generally useful

  @inline def put(sample: Double) = {
    val dev = sample - mean
    val newMean = mean + (dev / (count + 1))
    val newSumX2 = sumX2 + (dev * (sample - newMean))
    this.copy(count=count + 1, min=math.min(min, sample), max=math.max(max, sample), mean=newMean, sumX2=newSumX2, variance=(newSumX2 / (count + 1)))
  }
  def stddev = math.sqrt(variance)
}

object Sink {
  def empty: Sink = Sink(0, Double.PositiveInfinity, Double.NegativeInfinity, 0.0d, 0.0d, 0.0d)
}
