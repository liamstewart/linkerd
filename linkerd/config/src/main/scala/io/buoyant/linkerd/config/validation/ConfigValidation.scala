package io.buoyant.linkerd.config.validation

import io.buoyant.linkerd.config.validation.Validated.{Valid, Invalid}

/*
 * Implements a limited subset of cats' Validated, without the composability/correctness but also without requiring
 * an added dependency for linkerd.
 * Cats' license follows:
 */

/*
Cats Copyright (c) 2015 Erik Osheim.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

------

Code in Cats is derived in part from Scalaz. The Scalaz license follows:

Copyright (c) 2009-2014 Tony Morris, Runar Bjarnason, Tom Adams,
Kristian Domagala, Brad Clow, Ricky Clarkson, Paul Chiusano, Trygve
Laugstøl, Nick Partridge, Jason Zaugg. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
sealed trait Validated[+E, +A] extends Product with Serializable {
  def bimap[EE, AA](fe: Seq[E] => Seq[EE], fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply, fa andThen Valid.apply)

  def fold[B](fe: Seq[E] => B, fa: A => B): B =
    this match {
      case Invalid(es) => fe(es)
      case Valid(a) => fa(a)
    }

  // TODO: come up with a different name for this, since we're not implementing Applicative/Apply.
  def ap[EE >: E, B](f: Validated[EE, A => B]): Validated[EE, B] =
    (this, f) match {
      case (Valid(a), Valid(f)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
      case (e@Invalid(_), _) => e
      case (_, e@Invalid(_)) => e
    }

  def compose[EE >: E, B, C](other: Validated[EE, B])(f: (A, B) => C): Validated[EE, C] =
    this.ap(other.map(b => a => f(a, b)))

  def map[B](f: A => B): Validated[E, B] = bimap(identity, f)

  def toOption: Option[A] = fold(_ => None, Some.apply)

  def isValid: Boolean = fold(_ => false, _ => true)
  def isInvalid: Boolean = fold(_ => true, _ => false)

}

object Validated extends ValidatedFunctions {

  final case class Valid[+A](a: A) extends Validated[Nothing, A]

  final case class Invalid[+E](e: Seq[E]) extends Validated[E, Nothing]
}

object ValidationUtils {
  /*
   * Implements a non-generic way to turn Seq[Validated[A]] into Validated[Seq[A]].
   * TODO: how much extra typeclass mechanics would we need to include to allow this to be more generic?
   */
  def transpose[E, A](vs: Seq[Validated[E, A]]): Validated[E, Seq[A]] = {
    vs.foldLeft(Validated.valid[E, Seq[A]](Seq.empty[A])) {
      case (accum: Validated[E, Seq[A]], v) =>
        accum.ap(v.map(a => { s: Seq[A] => s :+ a }))
    }
  }

}

/*
 * Includes some useful helper methods.
 */
trait ValidatedFunctions {
  def invalid[A, B](a: A): Validated[A, B] = Validated.Invalid(Seq(a))

  def invalid[A, B](as: Seq[A]): Validated[A, B] = Validated.Invalid(as)

  def valid[A, B](b: B): Validated[A, B] = Validated.Valid(b)

}
