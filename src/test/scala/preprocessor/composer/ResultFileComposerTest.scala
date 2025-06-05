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
import dalculator.utils.{Configuration, FileManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.analysis.AnalysisTypes.{MCSAnalysis, TableAnalysis, TableLine}
import preprocessor.ast.ASTImplicits._
import preprocessor.ast.FC
import preprocessor.composer.all._
import preprocessor.transformers.all._

import scala.collection.immutable.SortedSet

class ResultFileComposerTest  extends AnyFlatSpec with should.Matchers {

  implicit val noWarning: Configuration = Configuration(printWarning = false)

  "The export" should "apply directly on any FC -> result map" in {
    Everywhere.on(MCSAnalysis(FC(Symbol("fc")), SortedSet(SortedSet(FailureMode("a.f"), FailureMode("b.f")))))
      .exportResult
      .value.path shouldBe FileManager.exportDirectory.getFile("fc.txt").getPath
  }

  "The export" should "apply directly on any table result" in {
    Everywhere.on(TableAnalysis(TableLine(FC(Symbol("fc")),1,2,"test") :: Nil))
      .exportResult
      .value.path shouldBe FileManager.exportDirectory.getFile("table_fc.txt").getPath
  }
}