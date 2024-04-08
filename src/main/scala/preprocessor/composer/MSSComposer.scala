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
import preprocessor.ast.ASTImplicits._
import preprocessor.ast.FC
import preprocessor.transformers.Parser
import preprocessor.transformers.Parser.{FCType, MSSType}
import preprocessor.transformers.all._

import scala.collection.immutable.{SortedSet => ISet}

trait MSSComposer[Pre] extends Composer [Pre]{
  type Result = MSSAnalysis
}

trait MSSComposerInstances {

  implicit def FileToMSSComposer(implicit parser:Parser.Aux[OCASFile,Option[(FC,List[MS])]]): MSSComposer[List[OCASFile]] = new MSSComposer[List[OCASFile]] {

    def apply(files: List[OCASFile]): MSSAnalysis = MSSAnalysis(files.flatMap(parse).toMap)

    private def parse(file: OCASFile): Option[(FC, MSS)] = {
      Everywhere.on(parser.parse(file))
        .map((x: List[MS]) => (ISet(x: _*), ISet.empty[CS]))
        .value
    }
  }

  implicit object MCSToMSSComposer extends MSSComposer[MCSAnalysis] {
    def apply(pre: MCSAnalysis): MSSAnalysis =
      Everywhere.on(pre)
        .map((mcs: MCS) => (ISet.empty[MS], mcs))
        .value
  }

}
