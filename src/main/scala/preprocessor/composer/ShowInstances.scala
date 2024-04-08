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

import preprocessor.analysis.AnalysisTypes.{DALAnalysis, MCSAnalysis, MSSAnalysis, ReliabilityAnalysis, TableAnalysis}
import cats.Show

trait ShowInstances {

  implicit object showDALAnalysis extends Show[DALAnalysis] {
     def show(t: DALAnalysis): String = t.result
       .map(p => s"DAL(${p._1.originalName}) = ${p._2}")
       .toSeq
       .sorted
       .mkString("\n")
  }

  implicit object showTable extends Show[TableAnalysis] {
    def show(t: TableAnalysis): String = t.result.mkString("\n")
  }

  implicit object showMCS extends Show[MCSAnalysis] {
    def show(t: MCSAnalysis): String =
      t.result.map(kv =>
        s"""cutOrders(${kv._1.name.name}) =
           |orders\t number
           |${kv._2.groupBy(_.size).map(p => s"${p._1}\t ${p._2.size}").toList.sorted.mkString("\n")}
           |total ${kv._2.size}
           |cuts(${kv._1}) =
           |${kv._2
          .toSeq
          .map(c => c.mkString("{", ",", "}"))
          .mkString("\n")}""".stripMargin
      ).mkString("\n")
  }


  implicit object showMSS extends Show[MSSAnalysis] {
    def show(t: MSSAnalysis): String =
      t.result.map(kv =>
        s"""cutOrders(${kv._1.name.name}) =
           |orders\t number
           |${kv._2._2.groupBy(_.size).map(p => s"${p._1}\t ${p._2.size}").toList.sorted.mkString("\n")}
           |total ${kv._2._2.size}
           |cuts(${kv._1}) =
           |${
          kv._2._2
          .toSeq
          .map(c => c.mkString("{", ",", "}"))
          .mkString("\n")}
           |
           |sequenceOrders(${kv._1.name.name}) =
           |orders\t number
           |${kv._2._1.groupBy(_.size).map(p => s"${p._1}\t ${p._2.size}").toList.sorted.mkString("\n")}
           |total ${kv._2._1.size}
           |sequences(${kv._1}) =
           |${
          kv._2._1.toSeq
            .map(_.mkString("(", ",", ")"))
            .mkString("\n")}""".stripMargin
      ).mkString("\n")
  }

  implicit object showReliability extends Show[ReliabilityAnalysis] {
    def show(t: ReliabilityAnalysis): String =
      t.result.map(kv =>
        s"${kv._1} = ${kv._2}").mkString("\n")
  }
}
