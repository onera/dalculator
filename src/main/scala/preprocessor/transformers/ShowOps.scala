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

import cats.Show
import cats.implicits.toShow
import preprocessor.transformers.EverywhereMap.OpsMap
import preprocessor.util.Ops
import shapeless.Poly1

trait ShowOps [T,W[_]] extends Ops[T,W] {
  self: OpsMap[T, W] =>

  def showAll (implicit everywhere: EverywhereMap[T, ShowOps.showPoly.type ]):W[everywhere.Result]= {
    map(ShowOps.showPoly)(everywhere)
  }
}

object ShowOps {
  object showPoly extends Poly1 {
    implicit def showAll[T : Show] = at[T](x => x.show)
  }
}