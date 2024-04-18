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

package preprocessor.composer

import dalculator.cli.DalculatorParameters
import dalculator.model.DalLevel.DalA
import dalculator.model.{AllocCstr, FailureCondition, FailureModeList, IndepRelation, Item, Model, UserDefinedConstraint, UserDefinedConstraints}
import dalculator.solver.SolveIndep
import preprocessor.analysis.AnalysisTypes.{MCSAnalysis, ResourceAnalysis}
import preprocessor.ast.FC

import scala.collection.mutable

trait ResourceComposer[Pre] extends Composer[Pre] {
  type Result = ResourceAnalysis
}

trait ResourceComposerInstances {

  implicit object SimpleResourceComposer extends ResourceComposer[(MCSAnalysis, Map[FC, Int], UserDefinedConstraints, Int, Int)] {
    def apply(pre: (MCSAnalysis, Map[FC, Int], UserDefinedConstraints, Int, Int)): ResourceAnalysis = {
      val params = new DalculatorParameters()
      params.indepResourcesStart = pre._4
      params.indepResourcesStop = pre._5
      val fm = new Model()
      val modelItems = mutable.Set.empty[Item]
      for {
        (fc, mcs) <- pre._1.result
        nSev = pre._2(fc)
      } yield {
        fm.add(FailureCondition(fc.name.name,
          nSev,
          -5, //FIXME DUMMY VALUE
          mcs.toList.map(cs => FailureModeList(cs.toList)),
          DalA //FIXME DUMMY VALUE
        ))
        modelItems ++= mcs.flatten.map(_.function)
      }
      if (fm.freeze)
        println("[WARNING] some MCS were discarded because they were shorter than nSev, DAL set to reference DAL.")

      println("Running Independence analysis")
      var result: Option[(IndepRelation, List[UserDefinedConstraint], List[AllocCstr])] = None
      var nofResources = params.indepResourcesStart
      while (result.isEmpty && nofResources <= params.indepResourcesStop) {
        println("Looking for independence relation with %d resources.".format(nofResources))
        result = SolveIndep(
          fm,
          pre._3,
          params.indepSolver,
          params.indepOpbFile,
          params.indepResultsFile,
          nofResources)
        nofResources += 1
      }
      if(result.isEmpty)
        println(s"Failed to produce allocation with ${params.indepResourcesStop}")

      ResourceAnalysis(result)
    }
  }
}
