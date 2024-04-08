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

/** Development assurance level for functions. */
abstract class DalLevel(val intValue: Int, val sRepr: String) extends Ordered[DalLevel] {

  def compare(that: DalLevel): Int = intValue compare that.intValue

  def -(that: Int): DalLevel = DalLevel((intValue - that) max DalLevel.DalE.intValue)

  def +(that: Int): DalLevel = DalLevel((intValue + that) min DalLevel.DalA.intValue)

  def to(that: DalLevel): Seq[DalLevel] = (intValue to that.intValue).map(DalLevel(_))

  def until(that: DalLevel): Seq[DalLevel] = (intValue until that.intValue).map(DalLevel(_))

  override def toString: String = sRepr
}

/** Companion object used for creating DalLevel instances */
object DalLevel {

  case object DalA extends DalLevel(4, "A")

  case object DalB extends DalLevel(3, "B")

  case object DalC extends DalLevel(2, "C")

  case object DalD extends DalLevel(1, "D")

  case object DalE extends DalLevel(0, "E")

  val values: Seq[DalLevel] = DalA :: DalB :: DalC :: DalD :: DalE :: Nil

  /** From a numerical value */
  def apply(i: Int): DalLevel = i match {
    case 4 => DalA
    case 3 => DalB
    case 2 => DalC
    case 1 => DalD
    case 0 => DalE
    case _ => throw new Exception("unrecognised dal level: " + i)
  }

  /** From a textual representation */
  def apply(name: String): DalLevel = name match {
    case "A" => DalA
    case "B" => DalB
    case "C" => DalC
    case "D" => DalD
    case "E" => DalE
    case _ => throw new Exception("unrecognised dal level: " + name)
  }
}