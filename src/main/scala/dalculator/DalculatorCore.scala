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

package dalculator

import dalculator.cli.DalculatorParameters
import dalculator.model._
import dalculator.solver.{SolveBudget, SolveDal, SolveIndep}
import dalculator.translator.ModelParser

/** The entry point of the dalculator. */
object DalculatorCore {

  /** Runs an analysis according to the given parameters. */
  def apply(params: DalculatorParameters): Unit = {
    this.clear()
    if (!(params.seqFiles.nonEmpty &&
      params.nSevs.length == params.seqFiles.length &&
      params.xSevs.length == params.seqFiles.length &&
      params.refDals.length == params.seqFiles.length)) {
      println("No sequence files specified, aborting.")
      System.exit(1)
    }

    // load cut files
    val fm = new Model
    for ((f, (nsev, (xsev, refDal))) <- params.seqFiles zip (params.nSevs zip (params.xSevs zip params.refDals))) {
      println("Parsing MCS with nSev='%d', xSev='%f': %s".format(nsev, xsev, f))
      ModelParser.loadFC(f, fm, nsev, xsev, refDal)
    }

    if (fm.freeze)
      println("Warning, some MCS were discarded because they were shorter than nSev, analysis may be unsound.")

    // do some work only if some MCS are of the proper size
    if (!(fm.mcs2DalnSevTuples.isEmpty & fm.mcsTooShort.isEmpty)) {

      if (params.indepEnabled) {
        // load user defined constraints
        if (params.indepUdefFile.isDefined) {
          println("Parsing user constraints: " + params.indepUdefFile)
          ModelParser.loadUDef(params.indepUdefFile.get, params.indepUdef)
        }

        println("Running Independence analysis")
        var result: Option[(IndepRelation, List[UserDefinedConstraint], List[AllocCstr])] = None
        var nofResources = params.indepResourcesStart
        while (result.isEmpty && nofResources <= params.indepResourcesStop) {
          println("Looking for independence relation with %d resources.".format(nofResources))
          // TODO modification leximin here
          result = SolveIndep(
            fm,
            params.indepUdef,
            params.indepSolver,
            params.indepOpbFile,
            params.indepResultsFile,
            nofResources)
          nofResources += 1
        }

        result match {
          case Some((indepRel, indepRelUdef, alloc)) =>
            println("Found a satisfying independence relation with %d resources".format(nofResources - 1))
            params.indepRelation = Some(indepRel)
            params.indepRelationUdef = Some(indepRelUdef)
            params.indepAlloc = Some(alloc)
            println("Writing results to file: %s".format(params.indepResultsFile))
            val toto = new UserDefinedConstraints
            indepRelUdef.foreach(c => toto.add(c))
            if (params.indepSaveAlloc)
              alloc.foreach(c => toto.add(c))
            toto.save(params.indepResultsFile)
          case None => throw new Exception("No satisfying independence relation found within given resource bounds.")
        }
      }

      if (params.dalEnabled) {
        // load user defined constraints
        for(f <- params.dalUdefFile) {
            println("Loading user constraints: " + f)
            ModelParser.loadUDef(f, params.dalUdef)
          }

        val result = SolveDal(fm,
          if (params.dalImportIndep) params.indepRelation else None,
          params.dalUdef,
          params.dalSolver,
          params.dalOpbFile,
          params.dalRule,
          params.dalResultsFile)

        if (result.isDefined) {
          params.dalAllocUdef = result
          println("Writing results to file: %s".format(params.dalResultsFile))
          val toto = new UserDefinedConstraints
          toto.dalRule = DalRuleConstraint(params.dalRule)
          params.dalAllocUdef.get.foreach(c => toto.add(c))
          toto.save(params.dalResultsFile)
        } else {
          throw new Exception("No satisfying DAL allocation found.")
        }
      }

      if (params.budgetEnabled) {
        // load user defined constraints
        if (params.budgetUdefFile.isDefined) {
          println("Loading user constraints: " + params.budgetUdefFile)
          ModelParser.loadUDef(params.budgetUdefFile.get, params.budgetUdef)
        }
        if (params.budgetUdef.aircraftData.isEmpty) {
          println("Warning : Short range aircraft profile assumed by default.")
          params.budgetUdef.add(ShortRange)
        }
        println("Solving budget allocation constraints")
        SolveBudget(fm,
          params.budgetUdef,
          if (params.budgetImportIndep) params.indepRelation else None,
          params.budgetCriterion,
          params.budgetLambdaMapFile,
          params.budgetItvMapFile,
          params.budgetLPFile,
          params.budgetResultsFile)
      }
    } else {
      println("Error: all MCS smaller than nSev, the system is empty, no analysis could be done.")
    }
  }

  /** clears all static caches so that the core is ready for a new round */
  def clear(): Unit = {
    Item.clear
    FailureMode.clear()
  }

}
