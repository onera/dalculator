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

package dalculator.solver

import dalculator.model.DalLevel._
import dalculator.model._
import theory.Logic._
import theory.pb.solver.OpbSolver
import theory.pb.translator.VarNamer._

import scala.language.reflectiveCalls
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

object SolveDal {

  /**
   * Encode the acceptance of t in MCS by option 1 if t is mutually independent and at least one f_i \in t is at initial
   * dal level
   *
   * @param mcs    the minimal cutset
   * @param refDal the initial dal of the fc of mcs
   * @param t      the analysed tuple of items
   * @return expression encoding acceptance of t of mcs by option 1
   */
  private def dalOKInMCSWithOption1(mcs: List[Int], refDal: DalLevel, t: List[Int]): BoolExp = {
    dalOKinMCS(mcs, t, refDal) <->
      (
        If(indepFuncs(t))
          Then Or(t.map(f => itemHasDal(f, refDal)))
          Else And(t.map(f => itemHasDal(f, refDal)))
        )
  }

  /**
   * Encode the acceptance of t in MCS by option 2 if t is mutually independent and at least two f_i, f_j \in t are at initial
   * dal level minus 1
   *
   * @param mcs    the minimal cutset
   * @param refDal the initial dal of the fc of mcs
   * @param t      the analysed tuple of items
   * @return expression encoding acceptance of t of mcs by option 2
   */
  private def dalOKInMCSWithOption2(mcs: List[Int], refDal: DalLevel, t: List[Int]): BoolExp = {
    dalOKinMCS(mcs, t, refDal) <->
      (
        If(indepFuncs(t))
          Then ~LeInt(PopCount(t.map(f => itemHasDal(f, refDal - 1))), 1)
          Else And(t.map(f => itemHasDal(f, refDal)))
        )
  }

  private def DalCostBasedCriterion(id: Int, costMap: Map[(Int, DalLevel), Int]): IfInt = {
    If(itemHasDal(id, DalA)) Then costMap.getOrElse((id, DalA), 1) Else
      (If(itemHasDal(id, DalB)) Then costMap.getOrElse((id, DalB), 1) Else
        (If(itemHasDal(id, DalC)) Then costMap.getOrElse((id, DalC), 1) Else
          (If(itemHasDal(id, DalD)) Then costMap.getOrElse((id, DalD), 1) Else
            costMap.getOrElse[Int]((id, DalE), 1))))
  }

  /**
   * Cost function : minimise the sum of all dal levels
   * TODO assign complexity weights to functions and used them in the criterion.
   * TODO strange cost definition since itemHasDal(f.uid, dalMax) -> itemHasDal(f.uid, dalMax - 1) &  itemHasDal(f.uid, dalMax - 2)
   * we have cost(dalMax) = \sum_{d <= dalMax} dalMax.intValue
   *
   * @param id     the id of the item
   * @param dalMax the max dal level
   * @return an expression encoding the cost as the sum over all dal under the selected one
   */
  private def defaultDalCostBasedCriterion(id: Int, dalMax: DalLevel): List[IfInt] = List(
    If(itemHasDal(id, dalMax)) Then dalMax Else 0,
    If(itemHasDal(id, dalMax - 1)) Then dalMax - 1 Else 0,
    If(itemHasDal(id, dalMax - 2)) Then dalMax - 2 Else 0
  )


  /** Solves the dal allocation problem. */
  def apply(
             fm: Model,
             indepRelation: Option[IndepRelation],
             udef: UserDefinedConstraints,
             solver: OpbSolver,
             opbFile: String,
             dalRule: DalRule,
             outFile: String): Option[List[UserDefinedConstraint]] = {

    /* The DAL allocation problem is modeled as follows */

    /* Stores the dal allocation constraints of the model */
    var dalConstraints: List[BoolExp] = Nil

    // convert indep solution to constraints for dal model
    indepRelation match {
      case Some(r) =>
        println("Importing independence relation.")
        val sortedTrue = r.trueElements.map(_.sorted)
        val sortedFalse = r.falseElements.map(_.sorted)
        val sortedTuples = fm.nSevTuples.map(_.sorted)
        for (t <- sortedTuples)
          if (sortedTrue.contains(t)) {
            dalConstraints ::= indepFuncs(t)
          } else if (sortedFalse.contains(t)) {
            dalConstraints ::= ~indepFuncs(t)
          }
      case None =>
    }

    // Import user specified Dal constraints
    dalConstraints ::= genDal(udef.dalConstraints)

    /* Import user specified indep constraints as "pairwise indep" */
    for (c <- udef.indepConstraints) {
      dalConstraints ::= pairwiseIndepFuncs(c.functions.map(_.uid))
    }

    /* Import user specified coloc constraints as "not pairwise indep" */
    for (c <- udef.notIndepConstraints) {
      dalConstraints ::= ~pairwiseIndepFuncs(c.functions.map(_.uid))
    }

    /* Generate the constraints that define the paiwiseIndependence relation :
		 * indep(f1,...,fn) <-> (indep_f1_f2 & ... & indep_fn_fm)
		 * */
    for (t <- fm.nSevTuples) {
      dalConstraints ::= indepFuncs(t) <-> pairwiseIndepFuncs(t)
    }

    // map used to keep track of min and max dal levels a function is subject to
    val minMaxDal: mutable.HashMap[Int, (DalLevel, DalLevel)] = new mutable.HashMap()

    /**
     * All functions in the set of too short functions should have the reference dal
     * */
    for ((mcs, refDal, _) <- fm.mcsTooShort; f <- mcs) {
      dalConstraints ::= itemHasDal(f, refDal)
      // keep track of min and max dal values
      val (dalMin, dalMax) = minMaxDal.getOrElseUpdate(f, (refDal, refDal))
      minMaxDal.update(f, (dalMin min refDal, dalMax max refDal))
    }

    for ((mcs, (refDal, tuples)) <- fm.mcs2DalnSevTuples.iterator) {

      for (f <- mcs) {
        val (dalMin, dalMax) = minMaxDal.getOrElseUpdate(f, (refDal, refDal))
        minMaxDal.update(f, (dalMin min refDal, dalMax max refDal))
      }

      // at least one nSev tuple satisfies the DAL criterion.
      dalConstraints ::= Or(tuples.map(t => dalOKinMCS(mcs, t, refDal)))

      // For each minimal cut set mcs, there must be at least one of its nSev-tuple such that
      // either option1, option2 or combined is satisfied.
      for (t <- tuples) {
        dalConstraints ::= {
          dalRule match {
            case DalRule1 => dalOKInMCSWithOption1(mcs, refDal, t)
            case DalRule2 => dalOKInMCSWithOption2(mcs, refDal, t)
            case DalRuleCombined => dalOKInMCSWithOption1(mcs, refDal, t) | dalOKInMCSWithOption2(mcs, refDal, t)
          }
        }
      }
    }

    /* for each function f of the model:
    * generate implications between dal levels in decreasing order,
    * generate constraint enforcing at most dal level Max-2
    */

    for (f <- Item.iterator) {
      for (level <- DalE until DalA) {
        dalConstraints ::= itemHasDal(f.uid, level + 1) -> itemHasDal(f.uid, level)
      }
      val (_, dalMax) = minMaxDal.getOrElse(f.uid, (DalE, DalA))
      dalConstraints ::= itemHasDal(f.uid, dalMax - 2 max DalE)
    }

    // This is the definition of the new cost : we take into account
    // the cost added by the user and we define the criterion associated to the cost
    val usedDefinedCostMap: Map[(Int, DalLevel), Int] = udef.collect({ case Cost(item, dal, value) => (item.uid, dal) -> value }).toMap
    var usedDefinedCostCriterion: List[IntExp] = Nil
    var defaultCostCriterion: List[IntExp] = Nil
    for (f <- Item.iterator) {
      usedDefinedCostCriterion ::= DalCostBasedCriterion(f.uid, usedDefinedCostMap)
      val (_, dalMax) = minMaxDal.getOrElse(f.uid, (DalE, DalA))
      defaultCostCriterion :::= defaultDalCostBasedCriterion(f.uid, dalMax)
    }

    // solve the model
    // defaultCostCriterion
    /** We define here a variable dalModel which will allow the algorithm to always return a result and which lists all the optimizations that can be performed by the DALculator */
    val dalModel =
      udef.optCriteria match {
        case OptMax(DalCard(_)) :: _ => Maximize(Add(defaultCostCriterion)) subjectTo dalConstraints
        case OptMax(CostCriterion) :: _ => Maximize(Add(usedDefinedCostCriterion)) subjectTo dalConstraints
        case OptMin(CostCriterion) :: _ =>
          Minimize(Add(usedDefinedCostCriterion)) subjectTo dalConstraints
        case _ => Minimize(Add(defaultCostCriterion)) subjectTo dalConstraints
      }

    dalModel.solve(solver) match {
      case LogicModelUnsat | LogicModelUnknown => None
      case LogicModelSat(values) =>
        var dalAlloc = List[DalLevelCstr]()
        for (f <- Item) {
          var res = List[DalLevelCstr]()
          for (dal <- DalE :: DalD :: DalC :: DalB :: DalA :: Nil) {
            val v = itemHasDal(f.uid, dal)
            values.get(v) match {
              case Some(true) => res ::= DalLevelCstr(f, DalRelOp.Eq, dal)
              case _ => ()
            }
          }
          // if a dal was found, return it, otherwise it the dal imposed by the worst FC
          res match {

            case h :: _ =>
              //TODO Display only when optimizing cost
              /** We add this line in order to print to the user the different cost kept */
              for {c <- usedDefinedCostMap.get((f.uid, h.right))} yield {
                println(s"Cost of function ${f.originalName} = $c")
              }
              dalAlloc ::= h
            case Nil => dalAlloc ::= DalLevelCstr(f, DalRelOp.Eq, minMaxDal.getOrElse(f.uid, (DalE, DalA))._2)
          }
        }
        Some(dalAlloc)
    }
  }

  def allocCost(allocation:Seq[(Item, DalLevel)], udef:UserDefinedConstraints): Int = {
    val usedDefinedCostMap: Map[(Int, DalLevel), Int] =
      udef.collect({ case Cost(item, dal, value) => (item.uid, dal) -> value }).toMap
    allocation.collect {
      case (f, dal) if usedDefinedCostMap.contains((f.uid, dal)) => usedDefinedCostMap((f.uid, dal))
    } match {
      case h :: t => (h :: t).sum
      case Nil => 0
    }
  }
}
