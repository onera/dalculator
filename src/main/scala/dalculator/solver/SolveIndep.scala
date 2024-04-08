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

import dalculator.model._
import theory.Logic._
import theory.pb.solver.OpbSolver
import theory.pb.translator.VarNamer._
import scala.language.reflectiveCalls

/** Generates a pseudo boolean file for the indep problem, solves it
 * and returns the resulting relation or None if no solution
 * exists.
 * */
object SolveIndep {
  def apply(
             m: Model,
             udef: UserDefinedConstraints,
             solver: OpbSolver,
             opbFile: String,
             resultsFile: String,
             nofResources: Int): Option[(IndepRelation, List[UserDefinedConstraint], List[AllocCstr])] = {

    /* List of independence constraints derived from the model */
    var indepConstraints = List[BoolExp]()

    /* All mcs that are too short must keep their size */
    for ((mcs, _, _) <- m.mcsTooShort if mcs.length >= 2) {
      indepConstraints ::= pairwiseIndepFuncs(mcs)
    }

    /* At least 1 nSev-tuple of pairwise independent functions shall be satisfied for each MCS.
		 * Pairwise independence for a nSev tuple is the conjunction of independence of pairs
		 * */
    for ((_, (_, tuples)) <- m.mcs2DalnSevTuples) {
      indepConstraints ::= Or(tuples.map(x => indepFuncs(x)))
    }

    /* Define the pairwiseIndependence relation :
		 * indep(f1,...,fn) <-> (indep_f1_f2 & ... & indep_fn_fm)
		 * */
    for (tuple <- m.nSevTuples) {
      indepConstraints ::= indepFuncs(tuple) <-> pairwiseIndepFuncs(tuple)
    }

    /* Generate constraints saying that each function is allocated on only one resource.
		 * */
    for (f <- Item; uid = f.uid) {
      indepConstraints ::= PopCount((0 until nofResources).map(r => funcOnRes(uid, r)).toList) Eq 1
    }

    /* Generate constraints saying that if two functions fi and fj are independent, then their allocations must differ :
		 * for all fi, fj,  k in [0, nofResources-1] , indep_fi_fj -> ~(fi_on_res_k & fj_on_res_k);
		 */
    for ((fi, fj) <- m.pairs; i <- 0 until nofResources) {
      indepConstraints ::= indepFuncs(fi, fj) -> ~(funcOnRes(fi, i) & funcOnRes(fj, i))
    }

    /* Import user defined constraints */
    /* collocations (groups of functions that must be on a same resource) */
    for (c <- udef.colocConstraints) {
      indepConstraints ::= colocFuncs(c.functions.map(_.uid), nofResources)
    }

    /* groups of functions that must be independent */
    for (c <- udef.indepConstraints) {
      indepConstraints ::= pairwiseIndepFuncs(c.functions.map(_.uid))
    }


    /* Groups of functions that must be allocated on a given resource */
    for (c <- udef.allocConstraints; f <- c.functions.map(_.uid)) {
      indepConstraints ::= funcOnRes(f, c.resource)
    }

    val indepModel = Minimize(PopCount(m.pairs.map(p => indepFuncs(p._1, p._2)).toList)) subjectTo indepConstraints

    println("Solving independence constraints.")

    indepModel.solve(solver) match {
      case LogicModelUnsat | LogicModelUnknown =>
        println("No result")
        None
      case LogicModelSat(values) =>
        println("satisfiable " + values)
        // independence relation as user defined constraints
        var rel = List[UserDefinedConstraint]()

        // list of pairs and  in the independence relation
        var trueElements = Set[List[Int]]()

        // list of pairs not in the  independence relation
        var falseElements = Set[List[Int]]()

        // generate indep relation over pairs
        for ((p1, p2) <- m.pairs) {
          val l = p1 :: p2 :: Nil
          values.get(indepFuncs(l: _*)) match {
            case Some(true) =>
              rel ::= IndepCstr(l.map(Item.getFromUid))
              trueElements += l
            case Some(false) =>
              rel ::= NotIndepCstr(l.map(Item.getFromUid))
              falseElements += l
            case None => ()
          }
        }

        // translate found allocation to constraints
        var alloc = List[AllocCstr]()
        for (f <- Item; r <- 0 until nofResources) {
          values.get(funcOnRes(f.uid, r)) match {
            case Some(true) => alloc ::= AllocCstr(f :: Nil, r)
            case _ => ()
          }
        }
        Some((new IndepRelation(trueElements, falseElements), rel, alloc))
    }
  }
}
