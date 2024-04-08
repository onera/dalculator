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

import preprocessor.composer.ResultFileOps
import preprocessor.transformers.Canonize.CanonizeOps
import preprocessor.transformers.EverywhereFilter.OpsFilter
import preprocessor.transformers.EverywhereMap.OpsMap

object all extends CustomInstances with ParserInstances {

  case class Ops[T](value: T)
    extends OpsFilter[T, Ops]
      with OpsMap[T, Ops]
      with FailureModeOps[T, Ops]
      with EventSetTransformerOps[T, Ops]
      with ShowOps[T, Ops]
      with CanonizeOps[T, Ops]
      with ResultFileOps[T, Ops] {

    def wrap[U](x: U): Ops[U] = Ops(x)
  }

  case object Everywhere {

    def on[T](x: T): Ops[T] = Ops(x)

    def on[T](x: Ops[T]): Ops[T] = Ops(x.value)
  }
}
