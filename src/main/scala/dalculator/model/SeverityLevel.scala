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

package dalculator.model

/** Severity levels for failure conditions. */
abstract class SeverityLevel(val nSev: Int, val xSev: Int, val sRepr: String) {
  override def toString: String = sRepr
}

/** Used for creating Severity levels from their string representation or numerical value. */
object SeverityLevel {

  /** The MIN severity level */
  case object MIN extends SeverityLevel(1, 3, "MIN")

  /** The MAJ severity level */
  case object MAJ extends SeverityLevel(2, 5, "MAJ")

  /** The HAZ severity level */
  case object HAZ extends SeverityLevel(2, 7, "HAZ")

  /** The CAT severity level */
  case object CAT extends SeverityLevel(3, 9, "CAT")

  /** From a numerical value. MAJ
   * and HAZ both correspond to value 2, by default HAZ is returned.
   * */
  def apply(nSev: Int): SeverityLevel = nSev match {
    case 1 => MIN
    case 2 => HAZ
    case 3 => CAT
    case _ => throw new Exception("unrecognised severity level:" + nSev)
  }

  /** From a textual representation. */
  def apply(name: String): SeverityLevel = name match {
    case "MIN" => MIN
    case "MAJ" => MAJ
    case "HAZ" => HAZ
    case "CAT" => CAT
    case _ => throw new Exception("unrecognised severity level:" + name)
  }
}