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

import dalculator.model.DalLevel._

/** A failure condition is described by its list of minimal cuts and severity level */
case class FailureCondition(name: String, nSev: Int, xSev: BigDecimal, cuts: List[FailureModeList], refDal: DalLevel) extends Iterable[FailureModeList] {
  require(nSev == 2 || nSev == 3)
  require(xSev <= -1.0)
  require(xSev >= -12.0)
  require(DalE.intValue <= refDal.intValue && refDal.intValue <= DalA.intValue)
  val mcsSize: Int = cuts.length
  val iterator: Iterator[FailureModeList] = cuts.iterator
}
