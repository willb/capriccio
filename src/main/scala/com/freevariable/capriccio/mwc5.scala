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
 * State for a simple multiply-with-carry generator.
 * the algorithm is due to George Marsaglia:  http://groups.google.com/group/comp.lang.c/msg/e3c4ea1169e463ae
 */

case class MWC5State(x: Long, y: Long, z: Long, w: Long, v: Long) extends PRNGState[MWC5State] {
  def shift: (Int, MWC5State) = {
    val t = crop((x ^ (x >>> 7)))
    val nv = crop((v ^ (v << 6)) ^ (t ^ (t << 13)))
    val ny = crop(z)
    (crop((ny + ny + 1) * nv).toInt, copy(x=crop(y),y=crop(z),z=crop(w),w=crop(v),v=crop(nv)))
  }

  @inline private [this] def crop(l: Long): Long = { l & 0xffffffff }
}

object MWC5 {
  val monad = _monad[MWC5State]

  private def _monad[S] = new Monad[({type t[x] = State[S, x]})#t] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}
