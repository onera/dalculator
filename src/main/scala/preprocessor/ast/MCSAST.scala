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

package preprocessor.ast

import cats.Functor
import preprocessor.analysis.AnalysisTypes.Real

trait MCSAST

sealed trait Law {
  def toReliability : Real => Real
}

case object Always extends Law {
  def toReliability: Real => Real = _ => Real(1)
}

case class Exponential(l:Real) extends Law {
  def toReliability: Real => Real = t => Math.exp(- l*t)
}

sealed trait Sev extends Ordered[Sev]{
  val name:String
  def toInt:Int
  def compare(that: Sev): Int = toInt.compareTo(that.toInt)
}

case object CAT extends Sev {

  override val name: String = "CAT"

  override def toInt: Int = 4
}

case object HAZ extends Sev {

  override val name: String = "HAZ"

  override def toInt: Int = 3
}

case object MAJ extends Sev {

  override val name: String = "MAJ"

  override def toInt: Int = 2
}

case object MIN extends Sev {

  override val name: String = "MIN"

  override def toInt: Int = 1
}

case object NSE extends Sev {

  override val name: String = "NSE"

  override def toInt: Int = 0
}

object Sev {
  val values: Set[Sev] = Set(CAT,HAZ,MAJ,MIN,NSE)
  def fromString(s:String):Sev = s match {
    case CAT.name => CAT
    case HAZ.name => HAZ
    case MAJ.name => MAJ
    case MIN.name => MIN
    case NSE.name => NSE
  }
  def isSev(s:String):Boolean = Sev.values.exists(_.toString == s)
}

case class FC(name: Symbol) extends MCSAST

sealed trait Analysis[Result] extends MCSAST {
  val result: Result

  override def toString: String = s"Analysis($result)"
}

object Analysis {

  implicit object AnalysisFunctor extends Functor[Analysis] {
    def map[A, B](fa: Analysis[A])(f: A => B): Analysis[B] = Analysis(f(fa.result))
  }

  def apply[T](outerResult: T): Analysis[T] = new Analysis[T] {
    val result: T = outerResult
  }
}
