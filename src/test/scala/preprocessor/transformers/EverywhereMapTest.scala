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
}