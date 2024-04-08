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

import scala.collection.immutable.{SortedSet => ISet}

trait FTAComposer[Pre] extends Composer[Pre]{
  type Result = FTA
}

trait FTAComposerInstances {
  implicit object MCSToFTAComposer extends FTAComposer[MCSAnalysis] {

    def apply(pre: MCSAnalysis): FTA = {

      //Analysis is invariant so result must be Tree
      val mkOr: ISet[Gate[FailureMode]] => Tree[FailureMode] = s => Gate(1, s.toList)

      Everywhere.on(pre)
        .map((e: FailureMode) => Leaf(e))
        .map((s: ISet[Leaf[FailureMode]]) => Gate(s.size, s.toList))
        .map(mkOr)
        .value
    }
  }

  implicit object XFTAToFTAComposer extends FTAComposer[FullXFTAFile] {
    def apply(pre: FullXFTAFile): FTA = ???
  }

}
