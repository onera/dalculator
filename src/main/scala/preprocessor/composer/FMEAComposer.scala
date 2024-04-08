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

import preprocessor.analysis.AnalysisTypes._
import preprocessor.ast.CAT

trait FMEAComposer[Pre] extends Composer[Pre]{
  type Result = FMEA
}

trait FMEAComposerInstances {
  implicit object MCSToFMEAReverseComposer extends FMEAComposer[(FHA, MCSAnalysis)] {

    def apply(pre: (FHA, MCSAnalysis)): FMEA = {
      val (fha, mcs) = pre
      val events = mcs.result.values.flatten.flatten.toSet
      FMEA(events.map(e => e -> mcs.result.filter(_._2.exists(_.contains(e))).keys.maxBy(fc => fha.result.getOrElse(fc, CAT))).toMap)
    }
  }
}
