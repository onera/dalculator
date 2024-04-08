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

import scala.annotation.tailrec

object BudgetUtility {

  /** Enumerates the cartesian product of the given list */
  def enumProduct[A, B](dims: List[List[A]], convert: List[A] => B): List[B] = {
    @tailrec
    def f(dims: List[List[A]], prod: List[List[A]]): List[B] = dims match {
      case dim :: dims => prod match {
        case Nil => f(dims, dim.map(x => List(x)))
        case prod@_ => f(dims, prod.flatMap(prodElem => dim.map(dimElem => dimElem :: prodElem)))
      }
      case Nil => prod.map(prodElem => convert(prodElem.reverse))
    }

    f(dims, Nil)
  }
}
