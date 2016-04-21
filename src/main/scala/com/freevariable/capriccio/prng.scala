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

private [capriccio] object utils {
  def int2bytes(i: Int) = (
    (i & 0xff).toByte,
    (i >>> 8 & 0xff).toByte,
    (i >>> 16 & 0xff).toByte,
    (i >>> 24 & 0xff).toByte
  )
}

trait PRNG {
  def nextByte: Byte

  def nextInt = { 
    (0 to 4).map(_ => nextByte).foldLeft(0) { (acc, b) => (acc << 8) | b }
  }

  def nextLong = { 
    (0 to 8).map(_ => nextByte).foldLeft(0l) { (acc, b) => (acc << 8) | b }
  }

  def nextFloat = java.lang.Float.intBitsToFloat(nextInt)

  def nextDouble = java.lang.Double.longBitsToDouble(nextLong)  

  def nextBoolean = ((nextByte & 1) == 1)
}

trait PRNGState[T] {
  def shift: (Int, PRNGState[T])
}

case class PRNGStream[T](initialState: PRNGState[T]) {
  import utils._
  
  val bytes: Stream[Byte] = {
    streamHelper(initialState)
  }

  val iterator: Iterator[Byte] = bytes.iterator
  
  @inline private[this] def streamHelper(st: PRNGState[T]): Stream[Byte] = {
    val (next, nextState) = st.shift
    val (b1, b2, b3, b4) = int2bytes(next)
    b1 #:: b2 #:: b3 #:: b4 #:: streamHelper(nextState)
  }
}

case class StreamBackedPRNG[T](stream: PRNGStream[T]) extends PRNG {
  val iterator = stream.iterator

  def nextByte = iterator.next()
}

case class ScalaRandomBackedPRNG(seed: Long) extends PRNG {
  import utils._
  import scala.util.Random

  val iterator = stream.iterator

  def nextByte = iterator.next()

  private val stream = streamHelper(new Random(seed))

  @inline private[this] def streamHelper(rng: Random): Stream[Byte] = {
    val next = rng.nextInt
    val (b1, b2, b3, b4) = int2bytes(next)
    b1 #:: b2 #:: b3 #:: b4 #:: streamHelper(rng)
  }
}
