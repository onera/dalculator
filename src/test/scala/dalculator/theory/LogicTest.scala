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

package dalculator.theory

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import theory.Logic._
import theory.pb.solver.Sat4jBoth

class LogicTest extends AnyFlatSpec with should.Matchers {

  "The Logic DSL" should "be able to build some simple terms" in {
    val i1 = "x1" -> ~("y1" & "z1")
    val z = If("x3") Then ("toto" -> "a") Else "Tutu"
    val i2 = "x2" -> ~("y2" & "z2")
    val i3 = "x3" -> ~("y3" & "z3")
    val m = Minimize(PopCount("x1", "x2", "x3")) subjectTo(i1, i2, i3)
    m.solve(Sat4jBoth) match {
      case LogicModelSat(values) => println(values)
      case LogicModelUnsat => fail("the problem is UNSAT and should be SAT")
      case LogicModelUnknown => fail("the problem is UNKNOWN and should be SAT")
    }
  }
}
