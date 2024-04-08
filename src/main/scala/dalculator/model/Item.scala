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

import dalculator.utils.UniqueString
import dalculator.utils.{Stringpool, UniqueString}

/** Represents a function. */
class Item(val name: UniqueString) {
  def uid = name.uid

  override def toString: String = "Fun%03d".format(this.name.uid)

  def originalName = name.s

  def originalNameQuoted = "'" + originalName + "'"
}

/** Component factory, does unique numbering of instances. */
object Item extends Iterable[Item] {

  /** stringpool used to cache instances and assign unique identifiers to functions */
  val stringpool = new Stringpool

  /** iterates on all Components currently in the cache */
  val instances = new scala.collection.mutable.HashMap[Int, Item]()

  /** clears the cache of functions */
  def clear = {
    stringpool.clear();
    instances.clear()
  }

  /** From a String representing its name. */
  def apply(name: String): Item = {
    val us = stringpool.get(name)
    instances.getOrElseUpdate(us.uid, new Item(us))
  }

  /** From a UniqueString representing its name. */
  def apply(name: UniqueString): Item = {
    val us = stringpool.get(name.s)
    instances.getOrElseUpdate(us.uid, new Item(us))
  }

  /** From a String representing its name only if already existing. */
  def get(name:String): Option[Item] = {
    val us = stringpool.get(name)
    instances.get(us.uid)
  }

  /** Returns the UniqueString behind the Component object. */
  def unapply(f: Item): Option[UniqueString] = Some(f.name)

  /** Iterates over all funtions ever created */
  def iterator = instances.valuesIterator

  /** returns an existing function fetched from its UID if it exists, throws an exception otherwise */
  def getFromUid(uid: Int): Item = {
    instances.get(uid) match {
      case Some(f) => f
      case None => throw new Exception("No function with such UID:" + uid)
    }
  }

  /** Return the number of functions */
  def nbr = instances.size
}