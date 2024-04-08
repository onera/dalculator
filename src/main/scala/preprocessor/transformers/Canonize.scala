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

import dalculator.model.FailureMode
import preprocessor.analysis.AnalysisTypes._
import preprocessor.ast.ASTImplicits._
import preprocessor.ast.{Analysis, FC}
import EverywhereFilter.OpsFilter
import EverywhereMap.OpsMap
import preprocessor.util.{Fact, Ops}
import shapeless.{Cached, Poly1}

import scala.collection.immutable.{SortedSet => ISet}
import scala.collection.mutable.{SortedSet => MSet}

trait Canonize [T] {
  def canonize(x:T):T
  def canonize(x:Analysis[T]): Analysis[T] = Analysis(canonize(x.result))
}

object Canonize {

  trait CanonizeOps[T,W[_]] extends Ops[T,W] {
    self : OpsFilter[T,W] with OpsMap[T,W] =>

    def canonizeAll (implicit everywhere: EverywhereMap[T, CanonizeOps.polyCanonize.type ]):W[everywhere.Result]= {
      map(CanonizeOps.polyCanonize)(everywhere)
    }
  }

  object CanonizeOps {
    object polyCanonize extends Poly1 {
      implicit def canCanonize[T](implicit canonizer:Canonize[T]): polyCanonize.Case[T] {
        type Result = T
      } = at[T](canonizer.canonize)
    }
  }

  implicit def FCAnalysisCanonizer[T](implicit canonizer: Canonize[T]): Canonize[Map[FC,T]] = (x: Map[FC, T]) =>
    x.transform((_, y) => canonizer.canonize(y))

  implicit object MSSCanonizer extends Canonize[MSS] {
    def canonize(x: MSS): MSS = {
      val (iniSequences, iniCuts)= x
      val seqs = MSet.empty[List[FailureMode]]
      val cuts = MSet(iniCuts.toSeq:_*)
      iniSequences
        .groupBy(seq => ISet(seq:_*))
        .foreach(p => {
          if (p._2.size == Fact.fact(p._1.size))
            cuts += p._1
          else
            seqs ++= p._2
        })
      val newCuts= MCSCanonize.canonize(ISet(cuts.toSeq:_*))
      seqs.foreach(
        s =>
          if (newCuts.exists(c => c.subsetOf(s.toSet)))
            seqs -= s
          else
            seqs.foreach(s2 => if (s != s2 && s.containsSlice(s2)) seqs -= s))
      (ISet(seqs.toSeq: _*), newCuts)
    }
  }

  implicit object MCSCanonize extends Canonize[MCS] {
    def canonize(x: MCS): MCS = {
      val rmEmpty = x.filter(_.nonEmpty)
      ISet(
        rmEmpty
          .filter(c => rmEmpty.forall(c2 => !c2.subsetOf(c) || c2 == c))
          .toSeq: _*)
    }
  }
}
