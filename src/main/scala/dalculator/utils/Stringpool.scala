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
 * */
package dalculator.utils

import scala.collection.mutable

/** A string pool data type, which allows to maintain a set of UniqueStrings.
 * Each unique string is uniquely identified by an integer index.
 * */
class Stringpool extends Iterable[UniqueString] {

  /** The cache of unique-strings. */
  var strings: mutable.Map[String, UniqueString] = mutable.HashMap[String, UniqueString]()

  /** Number of strings in the string pool. */
  var n: Int = 0

  /** Iterates on the contents of the string pool. */
  def iterator: Iterator[UniqueString] = strings.iterator.map(_._2)

  /** Returns the unique string corresponding to the given string. */
  def get(s: String): UniqueString = {
    strings.get(s) match {
      case Some(us) => us
      case None =>
        this.n += 1
        val res = UniqueString(s, this.n)
        this.strings += res.s -> res
        res
    }
  }

  /** clears the string pool from all its contents */
  def clear(): Unit = {
    strings.clear()
    n = 0
  }
}

