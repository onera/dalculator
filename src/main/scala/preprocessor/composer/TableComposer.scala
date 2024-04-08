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

import dalculator.model.FailureMode
import preprocessor.analysis.AnalysisTypes.{MCSAnalysis, MSSAnalysis, TableAnalysis, TableLine}


trait TableComposer[Pre] extends Composer[Pre] {
  type Result = TableAnalysis
}

trait TableComposerInstances {

  implicit object MSSToTableComposer extends TableComposer[(MSSAnalysis, Map[String, String])] {

    def exportS(x:Iterable[FailureMode], dictionary: Map[String,String]): String = {
       x.map(e => dictionary.getOrElse(e.name.s,e.toString)).toList.sorted.mkString(" and ")
    }

    def apply(pre: (MSSAnalysis, Map[String, String])): TableAnalysis = {
      val (analysis, dictionary) = pre
      val result = analysis.result
        .toList
        .sortBy(_._1.name.name)
        .flatMap(r => {
          val lines = (r._2._1 ++ r._2._2).toList.map(x => (x.size,exportS(x,dictionary))).groupBy(x => x)
          lines.map(kv => TableLine(r._1,kv._2.size,kv._1._1,kv._1._2))
        })
      TableAnalysis(result.sorted)
    }
  }

  implicit object MCSToTableComposer extends TableComposer[(MCSAnalysis, Map[String, String])] {

    def exportS(x:Iterable[FailureMode], dictionary: Map[String,String]): String = {
      x.map(e => dictionary.getOrElse(e.originalName,e.originalName)).toList.sorted.mkString(" and ")
    }

    def apply(pre: (MCSAnalysis, Map[String, String])): TableAnalysis = {
      val (analysis, dictionary) = pre
      val result = analysis.result
        .toList
        .sortBy(_._1.name.name)
        .flatMap(r => {
          val lines = r._2.toList.map(x => (x.size,exportS(x,dictionary))).groupBy(x => x)
          lines.map(kv => TableLine(r._1,kv._2.size,kv._1._1,kv._1._2))
        })
      TableAnalysis(result.sorted)
    }
  }

}