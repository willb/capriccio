/*
 * simulator.scala
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

import com.freevariable.capriccio.{PRNG, PRNGState, utils}

trait Simulator[T] {
  val prng: PRNG
  def expectedMean: Double
  def expectedVariance: Double
  def next: T
}

class GenericSimulator[T](override val prng: PRNG, 
			  val gen: PRNG => T, 
			  override val expectedMean: Double, 
			  override val expectedVariance: Double)
                         (implicit ev: T => Double)
     extends Simulator[T] {

  private val sink = Sink.empty

  def mean = sink.mean
  def min = sink.min
  def max = sink.max
  def variance = sink.variance
  def count = sink.count
  def next = { val result = gen(prng); sink.put(result); result }
}
