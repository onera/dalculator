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

import cats.instances.{ListInstances, MapInstances, OptionInstances, TupleInstances}
import cats.{Bifunctor, Functor, FunctorFilter, Monad}
import preprocessor.ast.ASTImplicits.buildAllPurposeOrdering

import scala.collection.immutable.SortedSet

trait CustomInstances extends ListInstances
  with MapInstances
  with OptionInstances
  with TupleInstances {

  implicit object SomeFunction extends Functor[Some] {
    def map[A, B](fa: Some[A])(f: A => B): Some[B] = Some(f(fa.value))
  }

//  implicit object SortedSetMonad extends Monad[SortedSet] {
//    def pure[A](x: A): SortedSet[A] = SortedSet(x)
//
//    def flatMap[A, B](fa: SortedSet[A])(f: A => SortedSet[B]): SortedSet[B] =
//      fa.flatMap(f)
//
//    def tailRecM[A, B](a: A)(f: A => SortedSet[Either[A, B]]): SortedSet[B] = ???
//  }

  implicit object SortedSetFunctor extends Functor[SortedSet] {
    def map[A, B](fa: SortedSet[A])(f: A => B): SortedSet[B] = SortedSet(fa.map(f).toSeq: _*)
  }

  implicit object SortedSetFunctorFilter extends FunctorFilter[SortedSet] {

    def functor: Functor[SortedSet] = SortedSetFunctor

    def mapFilter[A, B](fa: SortedSet[A])(f: A => Option[B]): SortedSet[B] = SortedSet(functor.map(fa)(f).flatten.toSeq: _*)
  }

}
