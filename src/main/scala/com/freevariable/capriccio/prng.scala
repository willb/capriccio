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

  def int2byteStream(i: Int, rest: => Stream[Byte] = Stream.empty) =  {
    (i >>> 24 & 0xff).toByte #::
    (i >>> 16 & 0xff).toByte #::
    (i >>> 8 & 0xff).toByte #::
    (i & 0xff).toByte #:: rest
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
  type underlying = T
  def shift: (Int, PRNGState[T])
}

case class PRNGStream[T](initialState: PRNGState[T]) {
  import utils._

  val iterator: Iterator[Byte] = streamHelper(initialState).iterator
  
  private[this] def streamHelper(st: PRNGState[T]): Stream[Byte] = {
    int2byteStream(st.shift._1, streamHelper(st.shift._2))
  }
}


case class StreamBackedPRNG[T](stream: PRNGStream[T]) extends PRNG {
  val iterator = stream.iterator

  def nextByte = iterator.next()
}