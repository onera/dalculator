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

import cats.{Bifunctor, Functor, FunctorFilter}
import preprocessor.util.Ops

import scala.annotation.implicitNotFound

/**
  * Everywhere type class for applying a function type HF on T
  *
  * @tparam T  type on which function must be applied
  * @tparam HF type of the function
  */

@implicitNotFound("could not find EverywhereFilter implicit value of type ${T} on function $[A} => Option[${B}]")
trait EverywhereFilter[T, A, B] extends Tracer {
  type Result

  def mapFilter(x: T)(f: A => Option[B]): Result
}

/**
  * Terminal cases for Everywhere implicit computation
  */
trait EverywhereFilterBaseCases {

  implicit def nonApplicable[T, A, B]: EverywhereFilter.Aux[T, A, B, T] = new EverywhereFilter[T, A, B] {

    val ruleName: String = "id"
    type Result = T
    def mapFilter(x: T)(f: A => Option[B]): Result = trace(x,x)
  }
}

/**
  * Recursive cases for Everywhere implicit computation
  */
trait EverywhereFilterRecursiveCases extends EverywhereFilterBaseCases {

  implicit def recEverywhere[T[_], T2, A, B, O](implicit isFunc: Functor[T], isTrans: EverywhereFilter.Aux[T2, A, B, O]): EverywhereFilter.Aux[T[T2], A ,B, T[O]] = new EverywhereFilter[T[T2], A, B] {
    type Result = T[O]

    val ruleName: String = "rec"

    def mapFilter(x: T[T2])(f: A => Option[B]): Result = {
      val y = isFunc.map(x)(t2 => isTrans.mapFilter(t2)(f))
      trace(x, y)
    }
  }
}

trait EverywhereFilterBiApplicationCases extends EverywhereFilterRecursiveCases {
  implicit def biEverywhere[T[_, _], T2, T3, A, B, O1, O2](implicit isFunc: Bifunctor[T], isTrans2: EverywhereFilter.Aux[T2, A, B, O1], isTrans3: EverywhereFilter.Aux[T3, A, B, O2]): EverywhereFilter.Aux[T[T2, T3], A, B, T[O1, O2]] = new EverywhereFilter[T[T2, T3], A, B] {
    type Result = T[O1, O2]

    val ruleName: String = "bi"

    def mapFilter(x: T[T2, T3])(f:  A => Option[B]): Result = {
      val y = isFunc.bimap(x)(t2 => isTrans2.mapFilter(t2)(f), t3 => isTrans3.mapFilter(t3)(f))
      trace(x, y)
    }
  }
}

/**
  * Application cases for Implicit computation
  */
trait EverywhereFilterApplicationCases extends EverywhereFilterBiApplicationCases {

  implicit def applyFunEverywhereFilter[T[_], I <: A, A, B](implicit isFF: FunctorFilter[T]): EverywhereFilter.Aux[T[I], A, B, T[B]] = new EverywhereFilter[T[I], A , B] {
    type Result = T[B]

    val ruleName: String = "filter"

    def mapFilter(x: T[I])(f: A => Option[B]): T[B] = trace(x, isFF.mapFilter(x)(f))

  }
}

object EverywhereFilter extends EverywhereFilterApplicationCases{

  type Aux[T, A, B, O] = EverywhereFilter[T, A, B] {
    type Result = O
  }

  trait OpsFilter[T, W[_]] extends Ops[T,W]{

    def collect[ A, B](f: PartialFunction[A, B])(implicit  everywhere: EverywhereFilter[T,A, B]):W[everywhere.Result]=
      mapFilter(f.lift)(everywhere)

    def filter[A](f: A => Boolean)(implicit  everywhere: EverywhereFilter[T,A, A]):W[everywhere.Result]=
      mapFilter((a:A) => Some(a).filter(f))(everywhere)

    def mapFilter[A,B](f: A => Option[B])(implicit  everywhere: EverywhereFilter[T,A, B]):W[everywhere.Result]=
      wrap(everywhere.mapFilter(value)(f))

  }
}