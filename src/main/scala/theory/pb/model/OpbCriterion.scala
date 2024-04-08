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

import java.io.BufferedWriter
import scala.collection.immutable.Vector

/**
 * Base class for all optimization criteria.
 * */
sealed abstract class OpbCriterion {
  /** Writes a textual representation of the criterion to a writer.
   *
   * @param out Buffer to write to.
   * */
  def dump(out: BufferedWriter): Unit
}

/**
 * The 'Solve' criterion, just asks for a solution, no optimization is performed.
 * */
case object OpbSolve extends OpbCriterion {
  def dump(out: BufferedWriter): Unit = {
    out.write("* criterion : solve\n")
  }
}

abstract class OpbOptimize extends OpbCriterion {
  protected var comment: Option[String] = None

  def setComment(s: String): Unit = comment = Some(s)

  def getLitsAndCoeffs: Vector[(Int, Int)]

  def getCte: Int

  def getLitSum: LitSum

  def toConstraint(bound: Int): OpbConstraint
}

/**
 * The 'Minimize' criterion, asks the solve to minimize the integer value of the given sum of literals.
 * */
case class OpbMin(sum: LitSum) extends OpbOptimize {
  val (litsAndCoeffs, cte) = sum.toConstraint

  def getLitsAndCoeffs: Vector[(Int, Int)] = litsAndCoeffs

  def getCte: Int = cte

  def getLitSum: LitSum = sum

  def dump(out: BufferedWriter): Unit = {
    val delim = " "
    if (comment.isDefined) {
      out.write("* ")
      out.write(comment.get)
      out.write("\n")
    }
    out.write("min: ")
    if (cte != 0) {
      out.write("+" + cte.toString)
      out.write(delim)
      out.write("x1 ")
    }
    sum.litsAndCoeffs.foreach(
      x => {
        val (l, c) = x
        val (a, b) = if (l > 0) ("x", l) else ("~x", -l)
        out.write("%+d".format(c))
        out.write(delim)
        out.write(a)
        out.write(b.toString)
        out.write(" ")
      })
    out.write(";\n")
  }

  /**
   * Returns a constraint modeling that this criterion is inferior or equal to a fixed bound.
   * */
  def toConstraint(bound: Int): OpbConstraint = OpbConstraint(sum, OpbLe, bound)
}

/**
 * The 'Maximize' criterion, asks the solve to minimize the integer value of the given sum of literals.
 * */
case class OpbMax(sum: LitSum) extends OpbOptimize {
  val iSum: LitSum = sum.invert
  val (litsAndCoeffs, cte) = iSum.toConstraint

  def getLitsAndCoeffs: Vector[(Int, Int)] = litsAndCoeffs

  def getCte: Int = cte

  def getLitSum: LitSum = iSum

  def dump(out: BufferedWriter): Unit = {
    val delim = " "
    if (comment.isDefined) {
      out.write("* ")
      out.write(comment.get)
      out.write("\n")
    }
    out.write("min: ")
    if (cte != 0) {
      out.write("+" + cte.toString)
      out.write(delim)
      out.write("x1 ")
    }
    litsAndCoeffs.foreach(
      x => {
        val (l, c) = x
        val (a, b) = if (l > 0) ("x", l) else ("~x", -l)
        out.write("%+d".format(c))
        out.write(delim)
        out.write(a)
        out.write(b.toString)
        out.write(" ")
      })
    out.write(";\n")
  }

  /**
   * Returns a constraint modeling that this criterion is inferior or equal to a fixed bound.
   * */
  def toConstraint(bound: Int): OpbConstraint = {
    OpbConstraint(sum.invert, OpbLe, -bound)
  }
}
