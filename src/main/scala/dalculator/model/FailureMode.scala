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

import dalculator.cli.DefaultParameters
import dalculator.utils.{Stringpool, UniqueString}

/** Represents the failure mode 'name' of function 'function'. */
class FailureMode(val name: UniqueString, val function: Item) {

  def uid: Int = name.uid

  /** Unique string representation. */
  override def toString: String = function.toString + (if (active) "AMode%03d" else "LMode%03d").format(uid)

  def originalName: String = function.originalName + "." + name.s

  def originalNameQuoted: String = "'" + originalName + "'"

  /** True iff the failure mode is active (vs. latent) */
  var active: Boolean = true

  /** True iff the failure mode is latent (vs. active) */
  def latent: Boolean = !this.active

  /** Sets the mode as active. */
  def setActive(): FailureMode = {
    this.active = true;
    this
  }

  /** Sets the mode as latent. */
  def setLatent(): FailureMode = {
    this.active = false;
    this
  }

  /** Optional list of possible interval check values for this failure mode.
   * If None, a default value must be assumed.
   * */
  var checkIntervals: Option[List[Int]] = None

  /** Sets the list of possible interval check values for this mode. */
  def setCheckIntervals(l: Option[List[Int]]): FailureMode = {
    this.checkIntervals = l;
    this
  }

  /** Lower bound for the log(lambda) of the failure mode. */
  var lBound: BigDecimal = DefaultParameters.lambdaLowerBound

  /** Upper bound for the log(lambda) of the failure mode. */
  var uBound: BigDecimal = DefaultParameters.lambdaUpperBound

  /** Sets lambda bounds. */
  def setBounds(l: BigDecimal, u: BigDecimal): Unit = {
    require(DefaultParameters.lambdaLowerBound <= l)
    require(u <= DefaultParameters.lambdaUpperBound)
    require(l <= u)
    this.lBound = l
    this.uBound = u
  }
}

/** FailureMode factory, generates failure mode instances and while assigning unique IDs to them. */
object FailureMode extends Iterable[FailureMode] {

  /** string pool used to cache failure mode instances and assign unique identifiers */
  val stringpool = new Stringpool

  /** All FailureMode instances currently in the cache */
  val instances = new scala.collection.mutable.HashMap[(Int, Int), FailureMode]()

  /** clears the cache of failure modes */
  def clear(): Unit = {
    stringpool.clear()
    instances.clear()
  }

  /** Generates a failure mode 'name' for function 'func'. */
  def apply(name: String, func: Item): FailureMode = {
    val us = stringpool.get(name)
    instances.getOrElseUpdate((func.uid, us.uid), new FailureMode(us, func))
  }

  /** Simplified constructor, decomposes the given string to find 'func'.'name' */
  def apply(name: String): FailureMode = {
    val s = name.split('.')
    if (s.length <= 1) {
      println("[WARNING] FailureMode error: could not identify failure mode in function identifier because it is too short to be decomposed:" + name)
      println(s"[WARNING] Assuming failure mode is implicit and $name is the name of the item or a common cause failure")
      FailureMode("",Item(name))
    } else {
      // call normal constructor
      FailureMode(s(s.length - 1), Item(s.take(s.length - 1).mkString(".")))
    }
  }

  /** Deconstructor */
  def unapply(fm: FailureMode): Option[(UniqueString, Item)] = {
    Some((fm.name, fm.function))
  }

  /** Iterates on all instances of Failure modes that have been created. */
  def iterator: Iterator[FailureMode] = instances.valuesIterator

  /** Return the number of functions */
  def nbr: Int = instances.size

}