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

import theory.pb.solver.OpbSolver
import theory.pb.solver.{OpbSolver, OpbSolverResult}
import theory.pb.translator.OpbFileName

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.collection.immutable.Vector
import scala.math.abs

/**
 * Class representing a pseudo-boolean constraint system.
 *
 * @constructor
 * @param header      The header of the system.
 * @param criterion   The list of criteria to optimize in lexicographical order.
 * @param constraints All constraints of the system.
 * @param comment     A comment describing the system.
 * */
case class OpbModel(
                     header: OpbHeader,
                     criteria: List[OpbCriterion],
                     clauses1: Vector[Int],
                     clauses2: Vector[(Int, Int)],
                     clauses3: Vector[(Int, Int, Int)],
                     clausesN: Vector[List[Int]],
                     constraints: Vector[OpbConstraint],
                     comment: Vector[String] = Vector.empty) {

  /** Returns true if the model is a pure CNF. */
  def isPureCnf: Boolean = constraints.isEmpty


  /** Writes a literal to the given buffer. */
  private def dumpLit(l: Int, out: BufferedWriter): Unit = {
    if (l < 0) {
      out.write("~x")
      out.write((-l).toString)
    } else {
      out.write("x")
      out.write(l.toString)
    }
  }

  def printClausePositiveLits(ls: List[Int], out: BufferedWriter): Unit = {
    var k = 1
    for (l <- ls) {
      if (l >= 1) {
        out.write(" +1 ")
        dumpLit(l, out)
      } else if (l <= -1) {
        out.write(" -1 ")
        dumpLit(-l, out)
        k -= 1
      } else {
        throw new Exception()
      }
    }
    out.write(" >= %d;\n".format(k))
  }

  /** Writes a textual representation of the model to the given buffer.
   *
   * @param out BufferedWriter to write to.
   * */
  def dump(out: BufferedWriter): Unit = {
    this.header.dump(out)
    if (this.criteria.size > 1) {
      out.write("* #aggregation= lexico\n")
    }
    comment.foreach(c => {
      out.write("* ")
      out.write(c)
      out.write("\n")
    })

    this.criteria.foreach(_.dump(out))

    clauses1.foreach(l => {
      if (l >= 1) {
        out.write("+1 ")
        dumpLit(l, out)
        out.write(" >= 1;\n")
      } else {
        out.write("-1 ")
        dumpLit(-l, out)
        out.write(" >= 0;\n")
      }
    })

    clauses2.foreach(l => {
      printClausePositiveLits(List(l._1, l._2), out)
    })

    clauses3.foreach(l => {
      printClausePositiveLits(List(l._1, l._2, l._3), out)
    })

    clausesN.foreach(l => {
      printClausePositiveLits(l, out)
    })
    this.constraints.foreach(_.dump(out))
  }

  /**
   * Writes the clausal part of the OPB model to a file in dimacs syntax.
   *
   * @param out BufferedWriter to write to.
   * */
  def dumpCnf(out: BufferedWriter): Unit = {
    val nbVar =
      clauses1.foldLeft(0)((res, x) => res max abs(x)) max
        clauses2.foldLeft(0)((res, x) => res max abs(x._1) max abs(x._2)) max
        clauses3.foldLeft(0)((res, x) => res max abs(x._1) max abs(x._2) max abs(x._3)) max
        clausesN.foldLeft(0)((res, x) => {
          x.foldLeft(res)((res, x) => res max x) max res
        })

    val nbClauses = clauses1.size + clauses2.size + clauses3.size + clausesN.size

    out.write("p cnf %d %d\n".format(nbVar, nbClauses))

    clauses1.foreach(l => {
      out.write(l.toString)
      out.write(" 0\n")
    })

    clauses2.foreach(l => {
      out.write(l._1.toString)
      out.write(" ")
      out.write(l._2.toString)
      out.write(" 0\n")
    })

    clauses3.foreach(l => {
      out.write(l._1.toString)
      out.write(" ")
      out.write(l._2.toString)
      out.write(" ")
      out.write(l._3.toString)
      out.write(" 0\n")
    })

    clausesN.foreach(l => {
      l.foreach(x => {
        out.write(" ");
        out.write(x.toString)
      })
      out.write(" 0\n")
    })
  }

  /**
   * Writes a textual representation of the pseudo-boolean model to a file.
   *
   * @param filename Name of file to write to.
   * */
  def dumpCnfToFile(filename: String): Unit = {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      this.dumpCnf(out)
      out.close()
    } catch {
      case _: IOException => println("Could not write Model to file '%s'\n".format(filename))
    }
  }

  /**
   * Writes a textual representation of the pseudo-boolean model to a file.
   *
   * @param filename Name of file to write to.
   * */
  def dumpToFile(filename: String): Unit = {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      this.dump(out)
      out.close()
    } catch {
      case _: IOException => println("Could not write Model to file '%s'\n".format(filename))
    }
  }

  def solve(solver: OpbSolver, globalTimeout: Long = -1, resultTimeout: Long = -1): OpbSolverResult = {
    val name = OpbFileName()
    dumpToFile(name)
    solver.run(name, globalTimeout = globalTimeout, resultTimeout = resultTimeout)
  }
}