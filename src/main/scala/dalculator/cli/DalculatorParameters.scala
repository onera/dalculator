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

/**
 * @author Rémi Delmas
 * */
package dalculator.cli

/** Class containing all parameters used for configuring an analysis.
 * IO parameters:
 *  - seq files
 *  - severities
 *
 * Independence analysis:
 * - inputs:
 *   - enabled
 *   - number of resources min and max
 *   - user defined constraints
 *   - indep results file
 *     - outputs:
 *   - independence relation
 *   - resource allocation example
 *
 * Dal analysis parameters:
 * - inputs:
 *   - enabled
 *   - dal allocation rule
 *   - user defined constraints
 *   - import of independence results in addition to user constraints (watch out for contradictions)
 *   - dal results file
 *
 * - outputs:
 *   - dal levels
 *
 * Budget analysis parameters:
 * - inputs:
 *   - enabled
 *   - optimization criterion
 *   - user defined constraints
 *   - budget results file
 *   - import of independence results in addition to user constraints (watch out for contradictions)
 *
 *
 * Any user defined constraint file can contain:
 * - collocations
 * - independence
 * - dal levels
 * - aircraft profile
 * - latent failures declarations
 * - restricted check intervals for latent failures
 * - lambda bounds
 *
 * Information not relevant to a phase of the analysis is simply ignored during that phase.
 * */

import dalculator.model._
import dalculator.utils.FileManager
import theory.pb.solver.{OpbSolver, Sat4jBoth}

import java.io.File

class DalculatorParameters {

  //------------ Global parameters ----------------
  /** List of seq files used in the analysis */
  var seqFiles: List[String] = Nil
  /** List of severities corresponding to the seq files */
  var nSevs: List[Int] = Nil
  /** List of budgets corresponding to the seq files */
  var xSevs: List[BigDecimal] = Nil
  /** List of budgets corresponding to the seq files */
  var refDals: List[DalLevel] = Nil

  //----------- Indep parameters ----------------
  /** Independence analysis 'enabled' status */
  var indepEnabled: Boolean = false
  /** Name of the user defined constraints file used for the independence analysis */
  var indepUdefFile: Option[String] = None
  /** user defined constraints for indep */
  var indepUdef: UserDefinedConstraints = new UserDefinedConstraints
  /** Name of the OPB file for the independence problem */
  var indepOpbFile: String = FileManager.temporaryDirectory.getFile("indep.opb").getAbsolutePath
  /** Number of resources used as start value in the incremental search for an independence relation */
  var indepResourcesStart: Int = 4
  /** Number of resources used as stop value in the incremental search for an independence relation */
  var indepResourcesStop: Int = 10
  /** Output file for indep results */
  var indepResultsFile: String = FileManager.analysisDirectory.getFile("indepResults.udef").getAbsolutePath
  /** OPB solver used for the independence model */
  var indepSolver: OpbSolver = Sat4jBoth
  /** Independence relation computed by the analysis (optional) */
  var indepRelation: Option[IndepRelation] = None
  /** Independence relation computed by the analysis expressed as user defined constraints (optional) */
  var indepRelationUdef: Option[List[UserDefinedConstraint]] = None
  /** Allocation relation computed by the analysis (optional) */
  var indepAlloc: Option[List[AllocCstr]] = None
  /** Save the resource allocation to file if one is found. */
  var indepSaveAlloc: Boolean = false

  //----------- Dal parameters ----------------
  /** DAL analysis 'enabled' status */
  var dalEnabled = false
  /** If true, indep relation computed by indep analysis will be used */
  var dalImportIndep = false
  /** Name of the user defined constraints file used for the dal analysis */
  var dalUdefFile: List[String] = List.empty
  /** user defined constraints for DAL */
  var dalUdef: UserDefinedConstraints = new UserDefinedConstraints
  /** Name of the OPB file for the DAL problem */
  var dalOpbFile: String = FileManager.temporaryDirectory.getFile("dal.opb").getAbsolutePath
  /** Allocation rule */
  var dalRule: DalRule = DalRule1
  /** dal results file */
  var dalResultsFile: String = FileManager.analysisDirectory.getFile("dalResults.udef").getAbsolutePath
  /** OPB solver used for the DAL model */
  var dalSolver: OpbSolver = Sat4jBoth
  /** DAL allocation computed by the analysis expressed as user defined constraints (optional) */
  var dalAllocUdef: Option[List[UserDefinedConstraint]] = None

  //----------- Budget parameters ----------------
  /** Budget analysis 'enabled' status */
  var budgetEnabled = false
  /** If true, indep relation computed by indep analysis will be used */
  var budgetImportIndep = false
  /** Name of the user defined constraints file used for the budget analysis */
  var budgetUdefFile: Option[String] = None
  /** user defined constraints for DAL */
  var budgetUdef: UserDefinedConstraints = new UserDefinedConstraints
  /** Budget allocation optimisation criterion */
  var budgetCriterion: BudgetCriterion = GlobalMinCriterion
  /** Output file for budget analysis results */
  var budgetResultsFile: String = "budgetResults.txt"
  /** Budget allocation computed by the analysis expressed as user defined constraints (optional) */
  var budgetAllocUdef: Option[List[UserDefinedConstraint]] = None
  /** temporary file used to store backtranslation information about lambdas */
  var budgetLambdaMapFile: String = FileManager.temporaryDirectory.getFile("budget.lambda.map").getAbsolutePath
  /** temporary file used to store backtranslation information about check intervals */
  var budgetItvMapFile: String = FileManager.temporaryDirectory.getFile("budget.itv.map").getAbsolutePath
  /** temporary file used to store the linear program to be solved */
  var budgetLPFile: String = FileManager.temporaryDirectory.getFile("budget.lp").getAbsolutePath
}
