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

package theory.lp.translator

import theory.lp.model._
import theory.lp.model.MilpModel
import theory.lp.model.{MilpBinVar, MilpConditionalConstraint, MilpConstraint, MilpCriterion, MilpEq, MilpGe, MilpGt, MilpIntVar, MilpLe, MilpLit, MilpLt, MilpMax, MilpMin, MilpModel, MilpRealLit, MilpRelOp, MilpSolve, MilpSpecialOrderedSet, MilpSum, MilpVar}

import java.io.{BufferedWriter, FileWriter}

/** Prints an MilpModel to a file in lp_solve format */
object LPSolvePrinter {

  def writeMilpVar(file: BufferedWriter, v: MilpVar): Unit = file.write(v.toString)

  def writeMilpLit(file: BufferedWriter, v: MilpLit): Unit = file.write(v.toString)

  def writeMilpRelOp(file: BufferedWriter, op: MilpRelOp): Unit = op match {
    case MilpGe => file.write(">=")
    case MilpGt => file.write(">")
    case MilpEq => file.write("=")
    case MilpLe => file.write("<=")
    case MilpLt => file.write("<")
  }

  def writeMilpSum(file: BufferedWriter, sum: MilpSum): Unit = {
    val iMax = sum.coeffs.length - 1
    for ((x, i) <- (sum.coeffs zip sum.vars).zipWithIndex) {
      val (c, v) = x
      writeMilpLit(file, c)
      file.write(" ")
      writeMilpVar(file, v)
      if (i < iMax)
        file.write(" + ")
    }
  }

  def writeMilpConstraint(file: BufferedWriter, ctr: MilpConstraint): Unit = ctr match {
    case MilpConditionalConstraint(cond, weight, sum, op, k, label) =>
      if (label.isDefined) {
        file.write(label.get)
        file.write(": ")
      }
      writeMilpLit(file, weight)
      file.write(" ")
      writeMilpVar(file, cond)
      file.write(" + ")
      writeMilpSum(file, sum)
      file.write(" ")
      writeMilpRelOp(file, op)
      file.write(" ")
      writeMilpLit(file, MilpRealLit(BigDecimal(k.toString) + BigDecimal(weight.toString)))
      file.write(";")

    case MilpConstraint(sum, op, k, label) =>
      if (label.isDefined) {
        file.write(label.get)
        file.write(": ")
      }
      writeMilpSum(file, sum)
      file.write(" ")
      writeMilpRelOp(file, op)
      file.write(" ")
      writeMilpLit(file, k)
      file.write(";")
  }

  def writeSpecialOrderedSet(file: BufferedWriter, sos: MilpSpecialOrderedSet): Unit = {

    file.write("SOS: ")

    val iMax = sos.vars.length - 1
    for ((v, i) <- sos.vars.zipWithIndex) {
      writeMilpVar(file, v)
      if (i < iMax)
        file.write(", ")
    }
    file.write(" <= ")
    file.write(sos.order.toString)
    file.write(";")
  }

  def writeMilpCriterion(file: BufferedWriter, crit: MilpCriterion): Unit = crit match {
    case MilpSolve => file.write("max: ;")
    case MilpMax(sum) => file.write("max: "); writeMilpSum(file, sum); file.write(";")
    case MilpMin(sum) => file.write("min: "); writeMilpSum(file, sum); file.write(";")
  }

  def apply(model: MilpModel, filename: String): Unit = {
    val file = new BufferedWriter(new FileWriter(filename))

    // criterion
    writeMilpCriterion(file, model.criterion)
    file.write("\n\n")

    // lower bounds

    // upper bounds

    // all constraints
    for (ctr <- model.constraints) {
      writeMilpConstraint(file, ctr)
      file.write("\n")
    }

    // declaration of integer variables
    val intVars = model.declarations.filter(v => v.isInstanceOf[MilpIntVar]).toList
    val iMax = intVars.length - 1
    if (iMax >= 0) {
      file.write("\nint ")
      for ((v, i) <- intVars.zipWithIndex) {
        writeMilpVar(file, v)
        if (i < iMax)
          file.write(", ")
      }
      file.write(";\n")
    }

    // declaration of binary variables
    val binVars = model.declarations.filter(v => v.isInstanceOf[MilpBinVar]).toList
    val jMax = binVars.length - 1
    if (jMax >= 0) {
      file.write("\nbin ")
      for ((v, j) <- binVars.zipWithIndex) {
        writeMilpVar(file, v)
        if (j < jMax)
          file.write(", ")
      }
      file.write(";\n")
    }

    // declarations of special ordered sets
    if (model.specialOrderedSets.nonEmpty) {
      file.write("\nsos\n")
      for (sos <- model.specialOrderedSets) {
        writeSpecialOrderedSet(file, sos)
        file.write("\n")
      }
    }
    // done
    file.write("\n")
    file.close()
  }
}
