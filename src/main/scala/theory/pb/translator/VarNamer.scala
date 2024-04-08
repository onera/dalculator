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

package theory.pb.translator

import theory.Logic.{And, BoolExp, Ident, Iff, Implies, Or, intValueFromInt}
import dalculator.model.DalLevel.{DalA, DalB, DalE}
import dalculator.model._

import scala.annotation.tailrec

/** This class is a factory for pseudo boolean variables (the name
 * follows the pattern x<int>) used when modelling indep,
 * dal or budget allocation problems.
 * */
object VarNamer {

  /** Returns a variable representing the mutual independence of a list of functions */
  def indepFuncs(funcs: Int*): Ident = indepFuncs(funcs.toList)

  /** Returns a variable representing the mutual independence of a list of functions */
  def indepFuncs(funcs: List[Int]): Ident = Ident("indep_f" + funcs.sortWith(_ >= _).mkString("_f"))

  /** Returns a variable representing that function 'f' is allocated on ressource 'r' */
  def funcOnRes(f: Int, r: Int): Ident = Ident("f%d_on_r%d".format(f, r))

  /** Returns a variable representing that dal option 1 is satisfied on the given list of functions for the given 'nsev' */
  def dalOK(nsev: Int, funcs: List[Int]): Ident = Ident("dal_ok_nsev%d_f".format(nsev) + funcs.sortWith(_ >= _).mkString("_f"))

  /** Returns a variable representing that dal option 1 is satisfied on the given list of functions for the given 'nsev' in the context of a given mcs */
  def dalOKinMCS(mcs: List[Int], funcs: List[Int], refDal: DalLevel): Ident = Ident("dal_%s_ok_in_mcs_f%s_for_f%s".format(refDal.toString,
    mcs.sortWith(_ >= _).mkString("_f"),
    funcs.sortWith(_ >= _).mkString("_f")))


  /** Returns a variable representing that function 'f' has dal level 'dal' */
  def itemHasDal(f: Int, l: DalLevel): Ident = Ident("f%d_has_dal%d".format(f, l.intValue))

  /** Generates a list of constraints that break symmetries of allocations,
   * by adding clauses which model an allocation that is monotone over functions :
   * For all pairs of functions and all ri<rj:
   * (indep_fi_fj & fi < fj) -> (alloc(fi,ri) < alloc(fj,rj))
   */
  def breakSym(fi: Int, fj: Int, nofResources: Int): BoolExp = {
    // TODO Fixme
    var symConstraints: List[BoolExp] = Nil
    for (ri <- 0 until nofResources; rj <- 0 until ri) {
      symConstraints ::= (indepFuncs(fi, fj) & (fi Lt fj)) -> (funcOnRes(fi, ri) & funcOnRes(fj, rj))
    }
    And(symConstraints)
  }

  /** Generates constraints enforcing a colocation constraint on a tuple of functions
   * for all pairs of functions (fi, fj), for all resources rk,
   * alloc(fi, rk) <-> alloc(fj, rk)
   * */
  def colocFuncs(funcs: List[Int], nofResources: Int): BoolExp = {
    @tailrec
    def process(l: List[Int], c: List[BoolExp]): List[BoolExp] = {
      l match {
        case fi :: t =>
          var r = c
          for (fj <- t; ri <- 0 to nofResources) {
            r ::= funcOnRes(fi, ri) <-> funcOnRes(fj, ri)
          }
          process(t, r)
        case Nil => c
      }
    }

    And(process(funcs, Nil))
  }

  /** Generates constraints enforcing a same resource allocation for a list of functions */
  def funcsOnRes(funcs: List[Int], resource: Int): BoolExp = And(funcs.map(funcOnRes(_, resource)))

  /** Generates a constraint modelling the pairwise independence of all functions of the given list. */
  def pairwiseIndepFuncs(funcs: List[Int]): BoolExp = {
    val res: List[BoolExp] = Nil

    @tailrec
    def process(l: List[Int], c: List[BoolExp]): List[BoolExp] = {
      var res = c
      l match {
        case fi :: t =>
          for (fj <- t) {
            res ::= indepFuncs(fi, fj)
          }
          process(t, res)
        case Nil => c
      }
    }

    And(process(funcs, res))
  }

  /** Generates constraints modelling a user specified upper bound for dal level for a function. */
  def genDal(dals: List[DalCstr]): BoolExp = {
    @tailrec
    def process(l: List[DalCstr], c: List[BoolExp]): List[BoolExp] = {
      l match {
        case h :: t =>
          h match {
            case DalLevelCstr(left, DalRelOp.Lt, right) =>
              val x = ~itemHasDal(left.uid, right)
              process(t, x :: c)
            case DalLevelCstr(left, DalRelOp.Eq, right) =>
              val x = itemHasDal(left.uid, right)
              if (right < DalA) {
                val y = ~itemHasDal(left.uid, right + 1)
                process(t, x :: y :: c)
              } else {
                process(t, x :: c)
              }
            case DalLevelCstr(left, DalRelOp.Ge, right) =>
              val cc = itemHasDal(left.uid, right)
              process(t, cc :: c)


            /** Constraint DAL(function1) < DAL(function2) */
            case DalqIdentCstr(left, DalRelOp.Lt, right) =>
              //TODO Transform the DAL(function1 < function2) in PB constraints
              val constraint1 = (DalE to DalB).map(f = i => {
                val vlefti = itemHasDal(left.uid, i)
                val possiblegDAL = Or((i + 1 to DalA).map(j => itemHasDal(right.uid, j)).toList)
                Implies(vlefti, possiblegDAL)
              }).toList

              /** Here we also add the final part of the constraint v(f, 3) */
              process(t, (constraint1 :+ ~itemHasDal(left.uid, DalA)) ++ c)


            /** Constraint DAL(function1) = DAL(function2) */
            case DalqIdentCstr(left, DalRelOp.Eq, right) =>
              //TODO Transform the DAL(function1 = function2) in PB constraints
              val constraint2 = (DalE to DalA).map(i => {
                val vlefti = itemHasDal(left.uid, i)
                val vrighti = itemHasDal(right.uid, i)
                Iff(vlefti, vrighti)
              }).toList
              process(t, constraint2 ++ c)


            /** Constraint DAL(function1) >= DAL(function2) */
            case DalqIdentCstr(left, DalRelOp.Ge, right) =>
              //TODO Transform the DAL(function1 >= function2) in PB constraints
              val constraint3 = (DalE to DalB).map(i => {
                val vlefti = itemHasDal(left.uid, i)
                val possiblegDAL = And((i + 1 to DalA).map(j => ~itemHasDal(right.uid, j)).toList)
                And(vlefti, possiblegDAL)
              }).toList
              val result = Or(constraint3 :+ itemHasDal(left.uid, DalA))
              process(t, c :+ result)
          }
        case Nil => c
      }
    }

    And(process(dals, Nil))
  }
}
