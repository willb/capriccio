/*
 * prng.scala
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

trait PRNGState[T] {
  def shift: (Int, PRNGState[T])
}

case class PRNGStream[T](state: PRNGState[T]) {
  val bytes: Stream[Byte] = {
    streamHelper(state)
  }
  
  @inline private[this] def streamHelper(st: PRNGState[T]): Stream[Byte] = {
    val (next, nextState) = st.shift
    val (b1, b2, b3, b4) = int2bytes(next)
    b1 #:: b2 #:: b3 #:: b4 #:: streamHelper(nextState)
  }

  @inline private[this] def int2bytes(i: Int) = (
    (i & 0xff).toByte,
    (i >>> 8 & 0xff).toByte,
    (i >>> 16 & 0xff).toByte,
    (i >>> 24 & 0xff).toByte
  )    
}
