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

package com.freevariable.capriccio.mutable

import com.freevariable.capriccio.{PRNG, PRNGState, utils}

case class MutablePRNG[T](initialState: PRNGState[T]) extends PRNG {
  import utils._

  private var currentState = initialState
  private var bytes: Iterator[Byte] = (Stream.empty.iterator)

  def nextByte: Byte = {
    if(bytes.isEmpty) {
      val (ni, ns) = currentState.shift
      currentState = ns
      this.bytes = int2byteIt(ni)
    }
    bytes.next
  }
}

case class ScalaRandomBackedPRNG(seed: Long) extends PRNG {
  import utils._
  import scala.util.Random

  val rng = new Random(seed)
  private val bytes = Array.fill(1024)(0.toByte)
  private var it: Iterator[Byte] = Iterator.empty

  def nextByte = {
    if(it.isEmpty) {
      rng.nextBytes(bytes)
      it = bytes.iterator
    }
    it.next
  }
}