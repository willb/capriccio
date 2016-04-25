/*
 * uniform.scala
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

object Uniform {
  /**
   * Creates a simulator for the uniform distribution with an optional bias.
   */
  def apply(prng: PRNG, minVal: Option[Double] = None, maxVal: Option[Double] = None): GenericSimulator[Double] = {
    val (lb, ub, gen) = 
      (minVal orElse maxVal).map {_ => 
        val lower = minVal.getOrElse(0.0)
        val upper = maxVal.getOrElse(1.0)
        val scale = (upper - lower) + lower
        (lower, upper, {(prng: PRNG) => prng.nextDouble * scale})
      }.getOrElse((0.0, 1.0, {(prng: PRNG) => prng.nextDouble}))
    
    assert(ub > lb)
    
    new GenericSimulator(prng, gen, (ub + lb) / 2.0, (ub - lb) / 12.0)
  }
}
