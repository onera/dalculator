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

import dalculator.utils.FileManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.analysis.AnalysisTypes.{MCSAnalysis, OCASFile, TableAnalysis}
import preprocessor.composer.all._
import preprocessor.transformers.all._

import java.io.FileWriter
import scala.collection.immutable.SortedSet
import scala.io.Source

class EverywhereMapTest  extends AnyFlatSpec with should.Matchers {

  "The Everywhere" should "apply directly on basic cases" in {
    Everywhere.on("a")
      .map((s: String) => s"X$s")
      .value shouldBe "Xa"

    Everywhere.on("a" :: "b" :: "c" :: Nil)
      .map((s: List[String]) => s.mkString(""))
      .value shouldBe "abc"
  }

  "The Everywhere" should "apply on recursively if not applicable on type" in {
    Everywhere.on(Option("a" :: "b" :: "c" :: Nil))
      .map((s: String) => s"X$s")
      .value shouldBe Some(List("Xa", "Xb", "Xc"))

    //If the tuple is not explicitly provided, none of the rules apply
    Everywhere.on(Some(("a" :: "b" :: "c" :: Nil, 1 :: 2 :: 3 :: Nil)))
      .map((s: String) => s"X$s")
      .value shouldBe Some("Xa" :: "Xb" :: "Xc" :: Nil, 1 :: 2 :: 3 :: Nil)

    Everywhere.on(Some(SortedSet("a", "b", "c"), 1 :: 2 :: 3 :: Nil))
      .map((s: String) => s"X$s")
      .value shouldBe Some(SortedSet("Xa", "Xb", "Xc"), 1 :: 2 :: 3 :: Nil)
  }

  "For AVEMXL the dalculator" should "provide high level comments for AVEMXL cut sets" in {

    def exportSummarisedFFS(files: List[String], writer: FileWriter, replaces:Map[String,String]): Unit = {
      writer.write(
        Everywhere.on(files.map(OCASFile))
          .map((l: List[OCASFile]) => from(l).derive[MCSAnalysis])
          .map((mcs: MCSAnalysis) => from(mcs, replaces).derive[TableAnalysis])
          .showAll
          .value
      )
      writer.close()
    }

    def extractDictionary(dictionaryPath: String): Map[String, String] = {
      val s = Source.fromFile(dictionaryPath)
      val result = s.getLines().filterNot(_.contains("//")).toSeq.map(_.split(",")).collect { case Array(k, v) => k -> v }.toMap
      s.close()
      result
    }

    for {
      functionalCatSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/functionalAssessment/noLatentFailure/CAT.O.seq")
      functionalHazSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/functionalAssessment/noLatentFailure/HAZ.O.seq")
      functionalMajSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/functionalAssessment/noLatentFailure/MAJ.O.seq")
      functionalFiles = functionalCatSeq :: functionalHazSeq :: functionalMajSeq :: Nil
      physicalCatSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/physicalAssessmentWithSW/CAT.O.seq")
      physicalHazSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/physicalAssessmentWithSW/HAZ.O.seq")
      physicalMajSeq <- FileManager.extractResourceAsFile("sequences/expertises/avem/physicalAssessmentWithSW/MAJ.O.seq")
      physicalFiles = physicalCatSeq :: physicalHazSeq :: physicalMajSeq :: Nil
      highLevelDictionary <- FileManager.extractResourceAsFile("preprocessor/expertises/avem/highLevelCommentDictionary.txt")
      highLevelReplaces = extractDictionary(highLevelDictionary)
      highLevelFunctionResults = new FileWriter(FileManager.exportDirectory.getFile("high_level_summarised_FFS_functions.txt"))
      highLevelPhysicalResults = new FileWriter(FileManager.exportDirectory.getFile("high_level_summarised_FFS_physical.txt"))
      lowLevelDictionary <- FileManager.extractResourceAsFile("preprocessor/expertises/avem/lowLevelCommentDictionary.txt")
      lowLevelReplaces = extractDictionary(lowLevelDictionary)
      lowLevelFunctionResults = new FileWriter(FileManager.exportDirectory.getFile("low_level_summarised_FFS_functions.txt"))
      lowLevelPhysicalResults = new FileWriter(FileManager.exportDirectory.getFile("low_level_summarised_FFS_physical.txt"))
    } yield {
      exportSummarisedFFS(functionalFiles, highLevelFunctionResults, highLevelReplaces)
      exportSummarisedFFS(physicalFiles, highLevelPhysicalResults, highLevelReplaces)
      exportSummarisedFFS(functionalFiles, lowLevelFunctionResults, lowLevelReplaces)
      exportSummarisedFFS(physicalFiles, lowLevelPhysicalResults, lowLevelReplaces)

    }
  }
}