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

package theory.pb.model

import theory.pb.Globals.OpbCoeff

import scala.collection.immutable.Vector

/**
 * Represents a weighted sum of literals of the form  (c1.l1 + .. + cn.ln + cte)
 *
 * The sum is normalized so that:
 * - coefficients are strictly positive
 * - literals are different from 0.
 *
 * TODO check if the sum of coefficients can be greater than MAX_INT (as some solvers do not support this).
 * */
case class LitSum(litsAndCoeffs: Vector[(Int, Int)], cte: Int) {

  /**
   * Returns the sum of all coefficients, cte excepted.
   * */
  lazy val sigmaCoeffs: OpbCoeff = litsAndCoeffs.foldLeft(0)((res, x) => res + x._2)

  /**
   * Returns a human readable string representation of the sum.
   * */
  override def toString: String = litsAndCoeffs.map(
    x => {
      val (l, c) = x
      val (a, b) = if (l > 0) ("x", l) else ("~x", -l)
      val delim = " "
      "%+d".format(c) + delim + a + b.toString
    }).mkString(" ") + " " + cte.toString

  /**
   * Returns the LitSum equivalent to -1 * this
   * */
  lazy val size: Int = litsAndCoeffs.size

  lazy val invert: LitSum = this.mult(-1)

  /**
   * Returns the equivalent of this + that (concatenation of the sums).
   * */
  def add(other: LitSum): LitSum = LitSum(litsAndCoeffs ++ other.litsAndCoeffs, cte + other.cte)

  /** Multiplies the sum by a integer constant, while maintaining the normal form. */
  def mult(i: Int): LitSum = {
    if (i == 0) {
      LitSum(Vector.empty, 0)
    } else if (i > 0) {
      LitSum(litsAndCoeffs.map(x => (x._1, i * x._2)), i * cte)
    } else {
      val (lc, k) = litsAndCoeffs.foldLeft[(Vector[(Int, Int)], Int)]((Vector.empty, cte))((res, x) => {
        val (vec, k) = res
        val (l, c) = x
        (vec :+ (-l, -i * c), k + c)
      })
      LitSum(lc, i * k)
    }
  }

  /** Returns a filtered version of the sum in which constant literals and zero coefficients have been filtered,
   * and the sum is normalized to have positive literals.
   * */
  def toConstraint: (Vector[(Int, Int)], Int) = {
    litsAndCoeffs.foldLeft[(Vector[(Int, Int)], Int)]((Vector.empty, cte))((res, x) => {
      val (vec, k) = res
      val (l, c) = x
      if (l == 1)
        (vec, k + c)
      else if (l == -1 || c == 0)
        (vec, k)
      else if (l < 1)
        (vec :+ (-l, -c), k + c)
      else
        (vec :+ (l, c), k)
    })
  }
}
