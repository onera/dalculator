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

/** Represents a minimal cut set, i.e. a conjunction of failure modes
 * that must occur to cause a FailureCondition.
 * */
case class FailureModeList(failureModes: List[FailureMode]) extends Iterable[FailureMode] {
  def apply(i: Int) = failureModes(i)

  def iterator = failureModes.iterator

  override def toString = iterator.mkString("{", ",", "}")
}
