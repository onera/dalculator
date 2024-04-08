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

/**
 * @author Rémi Delmas
 *         date 2011/10/18
 * */
package theory.pb.solver

import theory.pb.model.LitSum

import scala.collection.immutable.HashMap

/** Allows to get the values taken by literals and LitSum:s in a model found by a solver. */
class ModelEvaluator(val litStrings: List[String]) {

  val model: HashMap[Int, Boolean] = {
    var map = new HashMap[Int, Boolean]
    for (l <- litStrings; v <- l.split(" ")) {
      if (v.startsWith("v")) {
        ()
      } else if (v.startsWith("-x")) {
        map += (v.drop(2).toInt -> false)
      } else if (v.startsWith("x")) {
        map += (v.drop(1).toInt -> true)
      } else {
        throw new Exception("Unexpected token in model string: " + v)
      }
    }
    map
  }

  /** Returns the value of a literal in the model or None if the literal does not appear in the solution. */
  def apply(lit: Int): Option[Boolean] = lit match {
    case 1 => Some(true)
    case -1 => Some(false)
    case x =>
      if (x > 0) {
        model.get(x)
      } else {
        model.get(-x) match {
          case Some(b) => Some(!b)
          case None => None
        }
      }
  }

  /** Returns the value of a LitSum in the model or None if some of the literals of the sum do not appear in the solution. */
  def apply(sum: LitSum): Option[Int] = {
    var result = sum.cte
    for (x <- sum.litsAndCoeffs) {
      apply(x._1) match {
        case Some(true) => result += x._2
        case Some(false) =>
        case None => return None
      }
    }
    Some(result)
  }
}
