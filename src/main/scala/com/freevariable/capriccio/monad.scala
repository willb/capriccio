/*
 * monad.scala
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

import scala.language.higherKinds

/* private[capriccio] */ trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

/* private[capriccio] */ trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  val >>= = flatMap _

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

/*
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
    case Nil => unit(Nil)
    case hd::tl => {
      val seqTail = sequence(tl)
      map2(hd, seqTail)((head, tail) => head::tail)
    }
  }
*/

  def sequence[A](lma: List[F[A]]): F[List[A]] = 
    map(lma.foldLeft(unit[List[A]](Nil))((acc, ls) => map2(ls, acc)((head, tail) => head::tail)))(ls => ls.reverse)

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]) = f(value)
}

object Id {
  val monad = new Monad[Id] {
    def unit[A](a: => A) : Id[A] = Id[A](a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }
}

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def monad[S] = new Monad[({type t[x] = State[S, x]})#t] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}
