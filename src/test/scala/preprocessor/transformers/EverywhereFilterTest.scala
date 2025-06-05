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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.transformers.all._

import scala.collection.immutable.SortedSet

class EverywhereFilterTest  extends AnyFlatSpec with should.Matchers {

  "The everywhere filter operator" should "apply directly on basic cases" in {
    Everywhere.on("a")
      .filter((s:String) => s.contains("b"))
      .value shouldBe "a"

    Everywhere.on("a" :: "b" :: "c" :: Nil)
      .filter((s:String) => s.contains("b"))
      .value shouldBe "b" :: Nil
  }

  it should "apply on recursively if not applicable on type" in {
    Everywhere.on(Option( "a" :: "b" :: "c" :: Nil))
      .map((s:String) => s"X$s")
      .filter((s:String) => s.contains("b"))
      .value shouldBe Some("Xb" ::Nil)

    Everywhere.on(Some(("a" :: "b" :: "c" :: Nil, 1 :: 2 :: 3 :: Nil)))
      .map((s:String) => s"X$s")
      .filter((s:String) => s.contains("b"))
      .value shouldBe Some( "Xb" :: Nil, 1 :: 2 :: 3 :: Nil)

    Everywhere.on(Some((SortedSet("a", "b", "c"), 1 :: 2 :: 3 :: Nil)))
      .map((s:String) => s"X$s")
      .filter((s:String) => s.contains("b"))
      .value shouldBe Some(SortedSet("Xb"), 1 :: 2 :: 3 :: Nil)
  }
}