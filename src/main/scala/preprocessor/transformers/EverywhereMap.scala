/*
 * Copyright (c)  2023. ONERA
 * This file is part of Dalculator
 *
 * Dalculator is free software ;
 * you can redistribute it and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation ;
 * either version 2 of  the License, or (at your option) any later version.
 *
 * PML Analyzer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY ;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program ;
 *  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package preprocessor.transformers

import cats.{Bifunctor, Functor, Monoid}
import preprocessor.util.Ops
import shapeless.PolyDefns.Case
import shapeless.{::, HNil, Poly1}

import scala.annotation.implicitNotFound

trait Tracer {
  val ruleName: String
  def trace[A,B](x: A, y: B): B = {
//    println(s"$ruleName on $x => $y")
    y
  }
}

/**
  * Everywhere type class for applying a function type HF on T
  *
  * @tparam T  type on which function must be applied
  * @tparam HF type of the function
  */

@implicitNotFound("could not find Everywhere implicit value of type ${T} on function $[HF}")
trait EverywhereMap[T, HF] extends Tracer {
  type Result
  def map(x: T)(f: HF): Result
}

/**
  * Terminal cases for Everywhere implicit computation
  */
trait EverywhereMapBaseCases {

  implicit def nonApplicable[T, HF]: EverywhereMap.Aux[T, HF,T] = new EverywhereMap[T,HF]{
    type Result = T
    val ruleName: String = "id"
    def map(x: T)(f: HF): Result = trace(x,x)
  }
}

/**
  * Recursive cases for Everywhere implicit computation
  */
trait EverywhereMapRecursiveCases extends EverywhereMapBaseCases {

  implicit def recEverywhere[T[_], T2, HF, O](implicit isFunc: Functor[T], isTrans: EverywhereMap.Aux[T2, HF, O]): EverywhereMap.Aux[T[T2], HF, T[O]] = new EverywhereMap[T[T2], HF] {
    type Result = T[O]

    val ruleName: String = "rec"

    def map(x: T[T2])(f: HF): Result = {
      val y = isFunc.map(x)(t2 => isTrans.map(t2)(f))
      trace(x, y)
    }
  }
}


trait EverywhereMapBiRecursiveCases extends EverywhereMapRecursiveCases {
    implicit def biEverywhere[T[_, _], T2, T3, HF, O1, O2](implicit isFunc: Bifunctor[T], isTrans2: EverywhereMap.Aux[T2, HF, O1], isTrans3: EverywhereMap.Aux[T3, HF, O2]): EverywhereMap.Aux[T[T2, T3], HF, T[O1, O2]] = new EverywhereMap[T[T2, T3], HF] {
      type Result = T[O1, O2]

      val ruleName: String = "bi"

      def map(x: T[T2, T3])(f: HF): Result = {
        val y = isFunc.bimap(x)(t2 => isTrans2.map(t2)(f), t3 => isTrans3.map(t3)(f))
        trace(x, y)
      }
    }
}

/**
  * Application cases for Implicit computation
  */
trait EverywhereMapApplicationCases extends EverywhereMapBiRecursiveCases {

  implicit def mapFunEverywhere[T <: A, A, B]: EverywhereMap.Aux[T, A => B, B] = new EverywhereMap[T, A => B] {
    type Result = B

    val ruleName: String = "map fun"

    def map(x: T)(f: A => B): Result =  trace(x, f(x))

  }

  implicit def mapPolyEverywhere[T, HF <: Poly1, B](implicit c: Case.Aux[HF, T :: HNil, B]): EverywhereMap.Aux[T, HF, B] = new EverywhereMap[T, HF] {
    type Result = B

    val ruleName: String = s"map poly"

    def map(x: T)(f: HF): Result = trace(x, c(x :: HNil))
  }
}

object EverywhereMap extends EverywhereMapApplicationCases {

  type Aux[T, HF, O] = EverywhereMap[T, HF] {
    type Result = O
  }

  trait OpsMap[T,W[_]] extends Ops[T,W]{

    def map[F<:Poly1](f: F)(implicit everywhere: EverywhereMap[T,F]): W[everywhere.Result] =
      wrap(everywhere.map(value)(f))

    def foreach[A](f: A => Unit)(implicit everywhere: EverywhereMap[T,A => Unit]): W[everywhere.Result] =
      map(f)(everywhere)

    def map[A,B](f: A => B)(implicit everywhere: EverywhereMap[T,A => B]): W[everywhere.Result] =
      wrap(everywhere.map(value)(f))

    def map[A](f: PartialFunction[A,A])(implicit everywhere: EverywhereMap[T,A => A]): W[everywhere.Result] =
      map((x:A) => f.lift(x).getOrElse(x))(everywhere)

    def map[A,B](f:PartialFunction[A,B])(implicit isM:Monoid[B], everywhere: EverywhereMap[T,A => B]) : W[everywhere.Result] =
      map((x:A) => f.lift(x).getOrElse(isM.empty))(everywhere)
  }
}

