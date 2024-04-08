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
import preprocessor.analysis.AnalysisTypes._
import preprocessor.transformers.all._

trait OrderComposer[Pre]  extends Composer[Pre] {
  type Result = OrderAnalysis
}

trait OrderInstances {

  implicit object MCSToOrderSizeComposer extends OrderComposer[MCSAnalysis] {
    def apply(pre: MCSAnalysis): OrderAnalysis =
      Everywhere.on(pre)
        .map((mcs: MCS) => if (mcs.isEmpty) None else Some(mcs.maxBy(_.size).size))
        .value
  }

  implicit object MSSToOrderSizeComposer extends OrderComposer[MSSAnalysis] {
    def apply(pre: MSSAnalysis): OrderAnalysis =
      Everywhere.on(pre)
        .map((mss: Iterable[Iterable[FailureMode]]) => if (mss.isEmpty) None else Some(mss.maxBy(_.size).size))
        .map((p: (Option[Int], Option[Int])) => for (a <- p._1; b <- p._2) yield a.min(b))
        .value
  }

}
