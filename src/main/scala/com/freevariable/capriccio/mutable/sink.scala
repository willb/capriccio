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

package com.freevariable.capriccio.mutable

/**
 * On-line mean and variance estimates for a stream of Double values.
 * Uses the technique from <a href="http://dl.acm.org/citation.cfm?id=359153">"Updating mean and variance estimates: an improved method"</a>, by D. H. D. West (1979).
*/
sealed class Sink(private var _count: Long, private var _min: Double, private var _max: Double, private var _mean: Double, private var _variance: Double, private var _sumX2: Double) {
  // TODO:  parameterize this over sample (at least), fractional (mean/variance), and integral (count) types
  
  // TODO:  rework to use Pébay's algorithm (support arbitrary moments, support combining stream estimates) to make this code more generally useful
  
  @inline def put(sample: Double) = {
    val dev = sample - _mean
    _mean = _mean + (dev / (count + 1))
    _sumX2 = _sumX2 + (dev * (sample - _mean))
    _count = count + 1
    _min = math.min(_min, sample)
    _max = math.max(_max, sample)
    _variance = _sumX2 / _count
  }
  
  @inline def mean = _mean
  @inline def count = _count
  @inline def min = _min
  @inline def max = _max
  @inline def variance = _variance
  
  def stddev = math.sqrt(variance)
  
  override def toString = s"Sink(count=$count, min=$min, max=$max, mean=$mean, variance=$variance)"
}

object Sink {
  def empty: Sink = new Sink(0, Double.PositiveInfinity, Double.NegativeInfinity, 0.0d, 0.0d, 0.0d)
}
