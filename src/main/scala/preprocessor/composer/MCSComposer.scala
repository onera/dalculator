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
import dalculator.utils.Configuration
import preprocessor.analysis.AnalysisTypes._
import preprocessor.ast.ASTImplicits._
import preprocessor.ast.FC
import preprocessor.transformers.Parser
import preprocessor.transformers.all._

import scala.collection.immutable.{SortedSet => ISet}

trait MCSComposer[Pre] extends Composer[Pre] {
   type Result = MCSAnalysis
}

trait MCSComposerInstances {

  implicit object MSSToMCSComposer extends MCSComposer[MSSAnalysis] {

    def apply(pre: MSSAnalysis): MCSAnalysis =
      Everywhere.on(pre)
        .map((mss: MSS) => mss._2)
        .value
  }

  implicit def OCASFileToMCSComposer(implicit parser: Parser.Aux[OCASFile,Option[(FC,List[List[FailureMode]])]], conf:Configuration): MCSComposer[List[OCASFile]] = new MCSComposer[List[OCASFile]] {

    def apply(files: List[OCASFile]): MCSAnalysis = MCSAnalysis(files.flatMap(parse).toMap)

    private def parse(file: OCASFile): Option[(FC, MCS)] = {
      Everywhere.on(parser.parse(file))
        .map((x: List[FailureMode]) => ISet(x: _*))
        .map((x: List[CS]) => ISet(x: _*))
        .value
    }
  }

}