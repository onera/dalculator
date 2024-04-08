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

package preprocessor.composer

import cats.Show
import dalculator.utils.FileManager
import preprocessor.analysis.AnalysisTypes.ResultFile
import preprocessor.transformers.EverywhereMap
import preprocessor.transformers.EverywhereMap.OpsMap
import preprocessor.util.Ops
import shapeless.Poly1

import java.io.FileWriter

trait ResultFileComposer[Pre] extends Composer[Pre]{
  type Result = ResultFile
}

trait ResultFileComposerInstances {

  implicit def showComposer[T](implicit show: Show[T], namer:Namer[T]) : ResultFileComposer[T] = (pre: T) => {
    val file = FileManager.exportDirectory.getFile(s"${namer(pre)}.txt")
    val o = new FileWriter(file)
    o.write(show.show(pre))
    o.close()
    ResultFile(file.getPath)
  }

}

trait ResultFileOps [T,W[_]] extends Ops[T,W] {
  self: OpsMap[T, W] =>

  def exportResult (implicit everywhere: EverywhereMap[T, ResultFileOps.exportPoly.type ]):W[everywhere.Result]= {
    map(ResultFileOps.exportPoly)(everywhere)
  }

}

object ResultFileOps {
  object exportPoly extends Poly1 {
    implicit def exportAll[T](implicit c:ResultFileComposer[T]) = at[T](x => c(x))
  }
}
