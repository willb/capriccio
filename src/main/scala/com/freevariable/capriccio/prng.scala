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
  @inline def int2bytes(i: Int) = (
    (i >>> 24 & 0xff).toByte,
    (i >>> 16 & 0xff).toByte,
    (i >>> 8 & 0xff).toByte,
    (i & 0xff).toByte
  )

  @inline def int2byteStream(i: Int) =  {
    (i >>> 24 & 0xff).toByte #::
    (i >>> 16 & 0xff).toByte #::
    (i >>> 8 & 0xff).toByte #::
    (i & 0xff).toByte #:: Stream.empty
  }

  @inline def int2byteIt(i: Int) =  {
    Array((i >>> 24 & 0xff).toByte, (i >>> 16 & 0xff).toByte, (i >>> 8 & 0xff).toByte, (i & 0xff).toByte).iterator
  }

}

trait PRNG {
  def nextByte: Byte

  def nextInt = { 
    ((0 to 3).map(_ => nextByte).foldLeft(0) { (acc, b) => (acc << 8) | (b & 0xff) })
  }

  def nextLong = { 
    (0 to 7).map(_ => nextByte).foldLeft(0l) { (acc, b) => (acc << 8) | (b & 0xff) }
  }

  def nextFloat = (nextInt & 0x00ffffff) / ((1 << 24).toFloat)

  def nextDouble = (((nextInt & 0x7ffffff).toLong << 27) + (nextInt & 0xfffffff)) / ((1L << 54).toDouble)

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
    val bit = int2byteIt(next)
    bit.toStream ++ streamHelper(nextState)
  }
}

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

case class StreamBackedPRNG[T](stream: PRNGStream[T]) extends PRNG {
  val iterator = stream.iterator

  def nextByte = iterator.next()
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
