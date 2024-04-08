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
 * Represents a pseudo boolean constraint the form  c1.l1 + .. + cn.ln <= cte,
 * where all coefficients are strictly positive and cte >= 0.
 * */
class OpbConstraint private(val litsAndCoeffs: Vector[(Int, Int)], val cte: Int, val comment: Option[String] = None) {
  val relOp: OpbRelOp = OpbGe

  /**
   * Returns a human readable string representation of the constraint.
   * */
  override def toString: String = {
    litsAndCoeffs.map(
      x => {
        val (l, c) = x
        val (a, b) = if (l > 0) ("x", l) else ("~x", -l)
        val delim = " "
        "%+d".format(c) + delim + a + b.toString
      }).mkString(" ") + " " + relOp.toString + " " + cte.toString + ";"
  }

  /**
   * Writes a string representation of the constraint to the given writer.
   * */
  def dump(out: BufferedWriter): Unit = {
    if (comment.isDefined) out.write("c " + comment.get)
    litsAndCoeffs.foreach(x => {
      val (l, c) = x
      val (a, b) = if (l > 0) ("x", l) else ("~x", -l)
      val delim = " "
      out.write("%+d".format(c))
      out.write(delim)
      out.write(a)
      out.write(b.toString)
      out.write(" ")
    })
    out.write(relOp.toString)
    out.write(" ")
    out.write(cte.toString)
    out.write(";\n")
  }
}

/**
 * Companion object for OpbConstraint.
 * */
object OpbConstraint {

  /**
   * Creates a Constraint equivalent to sum <op> cte, using the following transformation rules
   * {{{
   * - (ls + k) <= cte  ~>  -1*(ls + k) >= -cte       ~> ls' + k' >= -cte    ~> ls' >=  -cte - k'
   * - (ls + k) <  cte  ~>  -1*(ls + k) >= -cte + 1   ~> ls' + k' >= -cte +1 ~> ls' >=  -cte - k' + 1
   * - (ls + k) >= cte  ~>                                                   ~> ls  >=   cte - k
   * - (ls + k) >  cte  ~>                                                   ~> ls  >=   cte - k + 1
   * }}}
   * */
  def apply(sum: LitSum, op: OpbRelOp, cte: Int): OpbConstraint = {
    val (litsAndCoeffs, c) = op match {
      case OpbGe =>
        val (litsAndCoeffs, k) = sum.toConstraint
        (litsAndCoeffs, cte - k)
      case OpbGt =>
        val (litsAndCoeffs, k) = sum.toConstraint
        (litsAndCoeffs, cte + 1 - k)
      case OpbLe =>
        val (litsAndCoeffs, k) = sum.invert.toConstraint
        (litsAndCoeffs, -cte - k)
      case OpbLt =>
        val (litsAndCoeffs, k) = sum.invert.toConstraint
        (litsAndCoeffs, -cte + 1 - k)
    }
    new OpbConstraint(litsAndCoeffs, c)
  }

  /** Creates a normalized Constraint from a list of literals describing a clause. */
  def fromClause(lits: List[Int]): OpbConstraint = {
    OpbConstraint(LitSum(Vector.empty ++ lits.map(l => (l, 1)), 0), OpbGe, 1)
  }
}
