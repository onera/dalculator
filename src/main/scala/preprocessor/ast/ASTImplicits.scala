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

import scala.Ordering.{String => StringOrdering}

object ASTImplicits {

  implicit def buildAllPurposeOrdering[B]:Ordering[B]= new Ordering[B]{
    private def normalize[T](x:T): List[String] = x match {
      case l:Iterable[_] => l.map(e => normalize(e).mkString("(",",",")")).toList
      case e => e.toString :: Nil
    }
    def compare(x: B, y: B): Int = {
      val ls = normalize(x)
      val rs = normalize(y)
      if (ls.size.compare(rs.size) != 0)
        ls.size.compare(rs.size)
      else {
        ls.zip(rs)
          .foldLeft(0)((acc, pair) =>
            if (acc != 0) acc
            else StringOrdering.compare(pair._1, pair._2))
      }
    }
  }
}
