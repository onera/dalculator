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
package dalculator.model

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet}

/** A model is a collection of failure conditions. */
class Model extends Iterable[FailureCondition] {

  /** True if the model has been frozen */
  var frozen = false

  /** All failure conditions of the model. */
  var failureConditions: List[FailureCondition] = Nil

  /** Iterator on the failure conditions of the model. */
  def iterator = failureConditions.iterator

  /** Adds a failure condition to the model */
  def add(fc: FailureCondition): Model = {
    this.failureConditions ::= fc
    this
  }

  /** Set of all tuples of functions of length 'nSev' derived from
   * all minimal cut sets of all FC:s present in the model. A 'nSevTuple' is a
   * tuple of functions of size 'nSev' that are candidate for pairwise
   * independence in an allocation.
   * Value is valid only once the model has been frozen.
   */
  var nSevTuples = new HashSet[List[Int]]()

  /** Pairs of independent functions potentially implied by the nsev tuples.
   * Value is valid only once the model has been frozen.
   * */
  var pairs = new HashSet[(Int, Int)]()

  /** Maps each mcs with the list of nSev-tuples derived from it and
   * the dal level of its FC of origin.
   * */
  val mcs2DalnSevTuples = new HashMap[List[Int], Tuple2[DalLevel, List[List[Int]]]]()

  /** The set of all MCS that have been found too short with respect to their FC's nSev. */
  val mcsTooShort = new HashSet[Tuple3[List[Int], DalLevel, Int]]()

  /** Calls the function f on all groups of contiguous items of length N of the given iterable. */
  def ntuples[A: Manifest](t: Iterable[A], N: Int, f: Array[A] => Unit): Unit = {
    val iterators: Array[Iterator[A]] = new Array(N)
    val values: Array[A] = new Array(N)
    iterators(0) = t.iterator

    def enum(i: Int, N: Int): Unit = {
      if (i == N - 1) {
        for (x <- iterators(i)) {
          values(i) = x
          f(values)
        }
      } else {
        while (iterators(i).hasNext) {
          values(i) = iterators(i).next()
          val (it1, it2) = iterators(i).duplicate
          iterators(i) = it1
          iterators(i + 1) = it2
          enum(i + 1, N)
        }
      }
    }

    enum(0, N)
  }

  /** Generates  some tuples from the FC */
  def genTuples: Boolean = {
    var warning = false

    // processes a tuple of functions
    def processTuple(tuple: List[Int])(a: Array[Int]) = {
      val pair = (a(0) max a(1), a(0) min a(1))
      pairs += pair
      nSevTuples += tuple
    }

    // enumerate all nSev-tuples in each cut of each FC
    for (fc <- this.failureConditions;
         nSev = fc.nSev;
         refDal = fc.refDal;
         seq <- fc.iterator.map(_.map(_.function.uid))) {
      // list of tuples of size nSev of independent function ids derived from seq
      var indepTuples = List[List[Int]]()
      val seqAsList = seq.toList

      def addIndepTuple(len: Int)(a: Array[Int]) = {
        var res = List[Int]()
        for (i <- 0 to len - 1) {
          res ::= a(i)
        }
        // order the identifiers (equivalent to asserting the independence relation is symmetric)
        res = res.sortWith((x, y) => x <= y)
        indepTuples ::= res
        // enumerate all pairs contained in the nSev tuple
        ntuples[Int](res, 2, processTuple(res))
      }

      if (seqAsList.size >= nSev) {
        // if the MCS is of sufficient size, derive all nSev-tuples from it and store it.
        ntuples[Int](seqAsList, nSev, addIndepTuple(nSev))
        // if the MCS has been already processed in another failure condition
        // the transformation obtained with the worst DAL is considered
        //FIXME Note that nSev should be be consistent with refDal so we should prevent to specify
        //FIXME two nSev for the same refDal
        if(!mcs2DalnSevTuples.contains(seqAsList) || mcs2DalnSevTuples(seqAsList)._1 < refDal)
          mcs2DalnSevTuples.update(seqAsList, (refDal, indepTuples))
      } else {
        // if it is too short
        // enumerate all pairs
        for (f1 <- seqAsList; f2 <- seqAsList if f1 != f2) {
          pairs += Tuple2(f1 max f2, f1 min f2)
        }
        // store it the the short set
        mcsTooShort += Tuple3(seqAsList, refDal, nSev)
        warning = true
      }
    }
    warning
  }

  /** Freezes the instance once all FCs have been loaded */
  def freeze: Boolean = {
    require(!frozen)
    frozen = true
    println("Number of Failure Modes  = " + FailureMode.nbr)
    println("Number of Components = " + Item.nbr)
    genTuples
  }
}
