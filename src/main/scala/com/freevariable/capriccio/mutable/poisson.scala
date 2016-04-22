/*
 * poisson.scala
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

object Poisson {
  @annotation.tailrec private[this] def arrivals(prng: PRNG, t: Double, z: Double, acc: Long=0L): Long = t match {
    case _ if t > z => arrivals(prng, t * prng.nextDouble, z, acc + 1)
    case _ => acc
  }
  
  /**
   * Creates a simulator for the Poisson distribution with the given underlying PRNG and a lambda value of <tt>l</tt>.
   */
  def apply(prng: PRNG, l: Double): GenericSimulator[Long] = {
    val z = math.exp(-l)
    val gen = {(prng: PRNG) => arrivals(prng, prng.nextDouble, z)}

    new GenericSimulator(prng, gen, l, l) { def lambda = l }
  }
}
