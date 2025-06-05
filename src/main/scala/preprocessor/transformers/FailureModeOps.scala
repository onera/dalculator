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
import dalculator.utils.Configuration
import preprocessor.transformers.EverywhereFilter.OpsFilter
import preprocessor.transformers.EverywhereMap.OpsMap
import preprocessor.util.Ops

import scala.util.matching.Regex

trait FailureModeOps[T,W[_]] extends Ops[T,W] {
  self : OpsFilter[T,W] with OpsMap[T,W] =>

  def remove(regex:  Regex)(implicit  everywhere: EverywhereFilter[T,FailureMode, FailureMode]):W[everywhere.Result]= {
    filter((e:FailureMode) => regex.findFirstIn(e.name.s).isEmpty)(everywhere)
  }

  def replaceEvent(toReplace:String, by:String)(implicit  everywhere: EverywhereMap[T,FailureMode => FailureMode],conf:Configuration):W[everywhere.Result]= {
    map( (e:FailureMode) => e match {
      case e:FailureMode if e.name.s.contains(toReplace) =>
        FailureMode(by)
      case e => e
    })(everywhere)
  }

  def replaceAll(reps:Map[String,String])(implicit  everywhere: EverywhereMap[T,FailureMode => FailureMode],conf:Configuration):W[everywhere.Result]= {
    map((e: FailureMode) => e match {
      case e : FailureMode if reps.exists(r => e.name.s.contains(r._1)) =>
        FailureMode(reps.foldLeft(e.name.s)((acc, rep) => acc.replaceAll(rep._1, rep._2)))
      case e => e
    })(everywhere)
  }
}

