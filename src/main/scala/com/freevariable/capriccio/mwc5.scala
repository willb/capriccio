/*
 * mwc5.scala
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

package com.freevariable.capriccio

/**
 * State for a simple multiply-with-carry generator 
 * the algorithm is due to George Marsaglia:  http://groups.google.com/group/comp.lang.c/msg/e3c4ea1169e463ae
 */

case class MWC5State(x: Int, y: Int, z: Int, w: Int, v: Int) {
  def shift: (Int, MWC5State) = {
    val t = (x ^ (x >> 7))
    val nv = (v ^ (v << 6)) ^ (t ^ (t ^ 13))
    val ny = z
    ((ny + ny + 1) * v, copy(x=y,y=z,z=w,w=v,v=nv))
  }
}
