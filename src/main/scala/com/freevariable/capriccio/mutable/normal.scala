/*
 * normal.scala
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

object Normal {
  private [this] class SpareCell {
    var d: Double = 0.0
    var has: Boolean = false
    def put(v: Double) { d = v; has = true }
    def get(): Double = { has = false; d }
    def getOrRecalc(f: => (Double, Double)): Double = {
      if (has) {
        get()
      } else {
        val (ret, spare) = f
        put(spare)
        ret
      }
    }
  }
  
  // Marsaglia's polar method
  @annotation.tailrec def genHelper(prng: PRNG): (Double, Double) = {
    val u = prng.nextDouble * 2 - 1
    val v = prng.nextDouble * 2 - 1
    val r = u * u + v * v
    if (r == 0 || r >= 1) {
      genHelper(prng)
    } else {
      val c = math.sqrt(-2 * math.log(r) / r)
      (c * u, c * v)
    }
  }
  
  private def mkgen(mean: Double, stddev: Double) = {
    val dc = new SpareCell()
        
    def gen(prng: PRNG): Double = {
      val raw = dc.getOrRecalc(genHelper(prng))
      raw * stddev + mean
    }
    
    gen _
  }
  
  /**
   * Creates a simulator for the normal distribution
   */
  def apply(prng: PRNG, mean: Double = 0.0, stddev: Double = 1.0): GenericSimulator[Double] = {
    new GenericSimulator(prng, mkgen(mean, stddev), mean, stddev * stddev)
  }
}
