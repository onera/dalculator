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

import dalculator.cli.DefaultParameters
import dalculator.model.BudgetMode.{BudgetMode, WorstCase}
import dalculator.model._
import dalculator.utils.ExternalProcess
import theory.lp
import theory.lp.model.{MilpConditionalConstraint, MilpConstraint, MilpGe, MilpIntLit, MilpLe, MilpLowerBoundConstraint, MilpMax, MilpModel, MilpRealLit, MilpSpecialOrderedSet, MilpSum, MilpUpperBoundConstraint, minItvLatent, minLambdaActive, minLambdaGlobal, minLambdaLatent, minSumLambdaActive, minSumLambdaLatent}
import theory.lp.translator
import theory.lp.translator.{CutSelector, FailureSelector, LPSolveBackTranslator, LPSolvePrinter, LogLambdaFM}

import java.io.{BufferedWriter, FileWriter}
import scala.math.log10

/** Generates an MILP constraint system from a Model instance. */
object SolveBudget {
  def apply(model: Model, udef: UserDefinedConstraints, indepRelation: Option[IndepRelation], criterion: BudgetCriterion, lambdaMapFile: String, itvMapFile: String, lpFile: String, budgetResultFile: String): Unit = {
    val bm = genMilp(model, udef, indepRelation, criterion, lambdaMapFile, itvMapFile)
    println("MILP system generated, invoking solver.")
    LPSolvePrinter(bm, lpFile)
    val backTranslator = new LPSolveBackTranslator(lambdaMapFile, itvMapFile, budgetResultFile)
    System.getProperty("os.name") match {
      case null => println("Cannot perform analysis on unknown os")
      case s if s.toLowerCase.contains("windows") =>
        ExternalProcess.exec(s"binlib\\lp_solve\\lp_solve.exe $lpFile")(backTranslator.readline)
      case s if s.toLowerCase.contains("linux") || s.toLowerCase.contains("mac") =>
        ExternalProcess.exec(s"binlib/lp_solve/lp_solve $lpFile")(backTranslator.readline)
      case s => println(s"Cannot perform analysis on $s os")
    }

    backTranslator.closeFile()
  }

  /** Generates the cut selector variables for all configurations of the given cut */
  def genCutSelectors(fc: FailureCondition,
                      fcUid: Int,
                      mcs: FailureModeList,
                      mcsUid: Int,
                      defaultIntervalChecks: List[Int]): List[CutSelector] = {
    val dims = mcs.failureModes.map(fm => if (fm.active) 1 :: Nil else fm.checkIntervals.getOrElse(defaultIntervalChecks))
    BudgetUtility.enumProduct(dims, { pElem: List[Int] => CutSelector(fc, fcUid, mcs, mcsUid, pElem) })
  }

  /** This function implements the top-level budget allocation strategy, in which the total budget of the given failure condition is distributed to minimal cuts.
   * In this implementation, each cut gets the same share of the total budget log10(Bound*T0/fc.mcs.size)
   * */
  def allocateBudgetFractionForMCS(fc: FailureCondition, mc: FailureModeList, T0: Int): BigDecimal = fc.xSev + BigDecimal(log10(T0)) - BigDecimal(log10(fc.mcsSize))

  /** returns true if the */
  def toIntThrowOverflow(v: BigInt): Int = {
    if (v > BigInt(Int.MaxValue))
      throw new Exception("Arithmetic overflow when converting BigInt to Int: " + v.toString)
    else
      v.intValue
  }

  /** @todo */
  def toDoubleThrowOverflow(v: BigInt): Double = {
    val d = v.toString.toDouble
    val df = new java.text.DecimalFormat()
    val s = df.format(d)
    d
  }

  /** Instanciates the budget allocation constraint for the given cut selector */
  def genBudgetConstraint(fc: FailureCondition, cutSel: CutSelector, T0: Int, mode: BudgetMode = WorstCase): Option[MilpConstraint] = {
    val mcsBound = allocateBudgetFractionForMCS(fc, cutSel.mcs, T0)
    val mcs = cutSel.mcs
    mcs.size match {
      case 1 =>
        val lambda1 = LogLambdaFM(cutSel.mcs(0)).milpVar
        val cte = MilpRealLit(mcsBound - BigDecimal(log10(T0)))
        val vars = lambda1 :: Nil
        val coeffs = MilpRealLit(1.0) :: Nil
        val sum = MilpSum(coeffs, vars)
        Some(MilpConditionalConstraint(cutSel.milpVar, MilpRealLit(DefaultParameters.K1), sum, MilpLe, cte))
      case 2 =>
        val lambda1 = LogLambdaFM(cutSel.mcs(0)).milpVar
        val lambda2 = LogLambdaFM(cutSel.mcs(1)).milpVar
        val itv1 = BigInt(cutSel.itvs(0))
        val itv2 = BigInt(cutSel.itvs(1))
        val x = itv1 + itv2 - 1
        val xd = toDoubleThrowOverflow(x)
        val cte = MilpRealLit(mcsBound - BigDecimal(2.0) * BigDecimal(log10(T0)) - BigDecimal(log10(xd)))
        val vars = lambda1 :: lambda2 :: Nil
        val coeffs = MilpRealLit(1.0) :: MilpRealLit(1.0) :: Nil
        val sum = MilpSum(coeffs, vars)
        Some(MilpConditionalConstraint(cutSel.milpVar, MilpRealLit(DefaultParameters.K1), sum, MilpLe, cte))
      case 3 =>
        val lambda1 = translator.LogLambdaFM(cutSel.mcs(0)).milpVar
        val lambda2 = translator.LogLambdaFM(cutSel.mcs(1)).milpVar
        val lambda3 = translator.LogLambdaFM(cutSel.mcs(2)).milpVar
        val itv1 = BigInt(cutSel.itvs(0))
        val itv2 = BigInt(cutSel.itvs(1))
        val itv3 = BigInt(cutSel.itvs(2))
        val x = BigInt(1) - itv1 - itv2 - itv3 + itv1 * itv2 + itv1 * itv3 + itv2 * itv3
        val xd = toDoubleThrowOverflow(x)
        val cte = MilpRealLit(mcsBound - BigDecimal(3.0) * BigDecimal(log10(T0)) - BigDecimal(log10(xd)))
        val vars = lambda1 :: lambda2 :: lambda3 :: Nil
        val coeffs = MilpRealLit(1.0) :: MilpRealLit(1.0) :: MilpRealLit(1.0) :: Nil
        val sum = MilpSum(coeffs, vars)
        Some(MilpConditionalConstraint(cutSel.milpVar, MilpRealLit(DefaultParameters.K1), sum, MilpLe, cte))
      case 4 =>
        val lambda1 = translator.LogLambdaFM(cutSel.mcs(0)).milpVar
        val lambda2 = translator.LogLambdaFM(cutSel.mcs(1)).milpVar
        val lambda3 = translator.LogLambdaFM(cutSel.mcs(2)).milpVar
        val lambda4 = translator.LogLambdaFM(cutSel.mcs(3)).milpVar
        val itv1 = BigInt(cutSel.itvs(0))
        val itv2 = BigInt(cutSel.itvs(1))
        val itv3 = BigInt(cutSel.itvs(2))
        val itv4 = BigInt(cutSel.itvs(3))
        val x = -BigInt(1) + itv1 + itv2 + itv3 + itv4 - itv1 * (itv2 + itv3 + itv4) + itv2 * (itv3 + itv4) - itv3 * itv4 + itv1 * (itv2 * (itv3 + itv4) + itv3 * itv4) + itv2 * itv3 * itv4
        val xd = toDoubleThrowOverflow(x)
        val cte = MilpRealLit(mcsBound - BigDecimal(4.0) * BigDecimal(log10(T0)) - BigDecimal(log10(xd)))
        val vars = lambda1 :: lambda2 :: lambda3 :: lambda4 :: Nil
        val coeffs = MilpRealLit(1.0) :: MilpRealLit(1.0) :: MilpRealLit(1.0) :: MilpRealLit(1.0) :: Nil
        val sum = MilpSum(coeffs, vars)
        Some(MilpConditionalConstraint(cutSel.milpVar, MilpRealLit(DefaultParameters.K1), sum, MilpLe, cte))
      case _ => println("The MCS was discarded in budget alloc: " + cutSel.mcs); None
    }
  }

  /** Instantiates the residual risk constraint for the given cut selector */
  def genResidualRiskConstraint(mc: FailureModeList, T0: Int, mode: BudgetMode = WorstCase): MilpConstraint = {
    def accum(acc: (List[LogLambdaFM], BigDecimal), fm: FailureMode): (List[LogLambdaFM], BigDecimal) = {
      val (l, c) = acc
      (translator.LogLambdaFM(fm) :: l,
        BigDecimal(log10(1)) +
          BigDecimal(log10(2)) +
          BigDecimal(log10(2)) +
          BigDecimal(log10(T0)) +
          c)
    }

    val (vars, cte) = mc.iterator.filter(_.active).foldLeft((List[LogLambdaFM](), BigDecimal("0.0")))(accum)
    val bound = BigDecimal(log10(T0)) + DefaultParameters.residualRiskLimitationBound
    val sum = MilpSum(vars.map(_ => MilpIntLit(1)), vars.map(x => x.milpVar))
    val cstr = MilpConstraint(sum, MilpLe, MilpRealLit(bound - cte))
    cstr
  }

  /** Instantiates the latency limitation constraint for the given cut selector */
  def genLatencyLimitationConstraint(cutSel: CutSelector, T0: Int, mode: BudgetMode = WorstCase): MilpConstraint = {
    def accum(acc: (List[LogLambdaFM], BigDecimal), elem: (FailureMode, Int)): (List[LogLambdaFM], BigDecimal) = {
      val (l, c) = acc
      val (fm, itv) = elem
      (translator.LogLambdaFM(fm) :: l,
        BigDecimal(log10(itv)) +
          BigDecimal(log10(T0)) +
          c)
    }

    val (vars, cte) = (cutSel.mcs.iterator zip cutSel.itvs.iterator).filter(x => x._1.latent).foldLeft((List[LogLambdaFM](), BigDecimal("0.0")))(accum)
    val sum = MilpSum(vars.map(_ => MilpRealLit(1.0)), vars.map(x => x.milpVar))
    val cstr = MilpConditionalConstraint(cutSel.milpVar, MilpRealLit(DefaultParameters.K2), sum, MilpLe, MilpRealLit(DefaultParameters.latencyLimitationBound - cte))
    cstr
  }

  /** generates implications between cut selectors and function selectors */
  def genSelectorImplications(cutSel: CutSelector): List[MilpConstraint] = {
    val cutSelVar = cutSel.milpVar
    (cutSel.mcs zip cutSel.itvs).map(x => {
      val (fm, itv) = x
      val vars = List(cutSelVar, FailureSelector(fm, itv).milpVar)
      val cstr = MilpConstraint(MilpSum(List(MilpIntLit(-1), MilpIntLit(1)), vars), MilpGe, MilpIntLit(0))
      cstr
    }).toList
  }

  /** Generates the definitions of min variables for active failure modes */
  def genMinVariablesActive(model: Model, result: MilpModel, active: List[FailureMode]): MilpModel = {
    // min of active lambdas
    result.add(MilpUpperBoundConstraint(minLambdaActive, MilpRealLit(DefaultParameters.globalLambdaUpperBound)))
    result.add(MilpLowerBoundConstraint(minLambdaActive, MilpRealLit(DefaultParameters.globalLambdaLowerBound)))
    active.foreach(
      fm => {
        result.add(
          MilpConstraint(
            MilpSum(
              List(MilpRealLit(1.0), MilpRealLit(-1.0)),
              List(minLambdaActive, translator.LogLambdaFM(fm).milpVar)),
            MilpLe,
            MilpRealLit(0.0)))
      }
    )

    // min of the sum of all active lambdas
    result.add(MilpUpperBoundConstraint(minSumLambdaActive, MilpRealLit(DefaultParameters.globalLambdaUpperBound * active.length)))
    result.add(MilpLowerBoundConstraint(minSumLambdaActive, MilpRealLit(DefaultParameters.globalLambdaLowerBound * active.length)))
    result.add(
      MilpConstraint(
        MilpSum(
          MilpRealLit(1.0) :: active.map(_ => MilpRealLit(-1.0)),
          minSumLambdaActive :: active.map(fm => translator.LogLambdaFM(fm).milpVar)),
        MilpLe,
        MilpRealLit(0.0)
      )
    )
  }

  /** Generates the definitions of min variables for active failure modes */
  def genMinVariablesLatent(model: Model, udef: UserDefinedConstraints, result: MilpModel, latent: List[FailureMode]): Unit = {
    // min of latent lambdas
    result.add(MilpUpperBoundConstraint(minLambdaLatent, MilpRealLit(DefaultParameters.globalLambdaUpperBound)))
    result.add(MilpLowerBoundConstraint(minLambdaLatent, MilpRealLit(DefaultParameters.globalLambdaLowerBound)))
    latent.foreach(
      fm => {
        result.add(
          MilpConstraint(
            MilpSum(
              List(MilpRealLit(1.0), MilpRealLit(-1.0)),
              List(minLambdaLatent, translator.LogLambdaFM(fm).milpVar)),
            MilpLe,
            MilpRealLit(0.0)))
      }
    )

    // min of the sum of all latent lambdas
    // lower bound is needed when it is negative, otherwise the solver assumes a 0 lower bound
    result.add(MilpUpperBoundConstraint(minSumLambdaLatent, MilpRealLit(DefaultParameters.globalLambdaUpperBound * latent.length)))
    result.add(MilpLowerBoundConstraint(minSumLambdaLatent, MilpRealLit(DefaultParameters.globalLambdaLowerBound * latent.length)))

    result.add(
      MilpConstraint(
        MilpSum(
          MilpRealLit(1.0) :: latent.map(_ => MilpRealLit(-1.0)),
          minSumLambdaLatent :: latent.map(fm => translator.LogLambdaFM(fm).milpVar)),
        MilpLe,
        MilpRealLit(0.0)
      )
    )
    // min variable for interval checks of latent failures

    // determine interval of interval checks values over latent failures only
    val itvs = udef.aircraftData.get.checkIntervals

    // TODO add bounds on interval checks
    val (minItv, maxItv) = computeMinMax(FailureMode.iterator.filter(_.latent), itvs)
    result.add(MilpUpperBoundConstraint(minItvLatent, MilpIntLit(maxItv)))
    result.add(MilpLowerBoundConstraint(minItvLatent, MilpIntLit(minItv)))

    for (fm <- FailureMode.iterator.filter(_.latent)) {
      itvs.iterator.foreach(
        itv => result.add(
          MilpConditionalConstraint(translator.FailureSelector(fm, itv).milpVar,
            MilpIntLit(maxItv),
            MilpSum(List(MilpIntLit(1)), List(minItvLatent)),
            MilpLe,
            MilpIntLit(itv))))
    }

    // TODO constraints for the min of the sum of all interval checks (dependent of cutselectors)
    // result.add(
    // 	MilpConstraint(
    // 		MilpSum(
    // 			MilpRealLit(1.0)::latent.map(fm => MilpRealLit(-1.0)),
    // 			minSumItvLatent::latent.map(fm => LogLambdaFM(fm).milpVar)),
    // 		MilpLe,
    // 		MilpRealLit(0.0)
    // 	)
    // )
  }

  /** Generates the definitions of min variables for all failure modes */
  def genMinVariablesGlobal(model: Model, udef: UserDefinedConstraints, result: MilpModel, global: List[FailureMode]): Unit = {
    // min of all lambdas
    result.add(MilpUpperBoundConstraint(minLambdaGlobal, MilpRealLit(DefaultParameters.globalLambdaUpperBound)))
    result.add(MilpLowerBoundConstraint(minLambdaGlobal, MilpRealLit(DefaultParameters.globalLambdaLowerBound)))
    global.foreach(
      fm => {
        result.add(
          MilpConstraint(
            MilpSum(
              List(MilpRealLit(1.0), MilpRealLit(-1.0)),
              List(minLambdaGlobal, translator.LogLambdaFM(fm).milpVar)),
            MilpLe,
            MilpRealLit(0.0)))
      }
    )

    // // min of the sum of all lambdas
    // // lower bound is needed when it is negative, otherwise the solver assumes a 0 lower bound
    // result.add(MilpUpperBoundConstraint(minSumLambdaGlobal, MilpRealLit(DefaultParameters.globalLambdaUpperBound*latent.length)))
    // result.add(MilpLowerBoundConstraint(minSumLambdaGlobal, MilpRealLit(DefaultParameters.globalLambdaLowerBound*latent.length)))

    // result.add(
    // 	MilpConstraint(
    // 		MilpSum(
    // 			MilpRealLit(1.0)::latent.map(fm => MilpRealLit(-1.0)),
    // 			minSumLambdaLatent::latent.map(fm => LogLambdaFM(fm).milpVar)),
    // 		MilpLe,
    // 		MilpRealLit(0.0)
    // 	)
    // )
    // // min variable for interval checks of latent failures

    // // determine interval of interval checks values over latent failures only
    // val itvs = udef.aircraftData.get.checkIntervals

    // // TODO add bounds on interval checks
    // val (minItv, maxItv) = computeMinMax(FailureMode.iterator.filter(_.latent), itvs)
    // println ("min max " + (minItv, maxItv))
    // result.add(MilpUpperBoundConstraint(minItvLatent, MilpIntLit(maxItv)))
    // result.add(MilpLowerBoundConstraint(minItvLatent, MilpIntLit(minItv)))

    // for (fm <- FailureMode.iterator.filter(_.latent)) {
    // 	itvs.iterator.foreach(
    // 		itv => result.add(
    // 			MilpConditionalConstraint(FailureSelector(fm, itv).milpVar,
    // 									  MilpIntLit(maxItv),
    // 									  MilpSum(List(MilpIntLit(1)), List(minItvLatent)),
    // 									  MilpLe,
    // 									  MilpIntLit(itv))))
    // }

    // TODO constraints for the min of the sum of all interval checks (dependent of cutselectors)
    // result.add(
    // 	MilpConstraint(
    // 		MilpSum(
    // 			MilpRealLit(1.0)::latent.map(fm => MilpRealLit(-1.0)),
    // 			minSumItvLatent::latent.map(fm => LogLambdaFM(fm).milpVar)),
    // 		MilpLe,
    // 		MilpRealLit(0.0)
    // 	)
    // )
  }


  /** computes the min and the max of interval check values over all given failures (must be non empty) */
  def computeMinMax(failures: Iterator[FailureMode], defaultIntervalChecks: List[Int]): (Int, Int) = {
    require(failures.nonEmpty)

    def minMax(mM: (Int, Int), fm: FailureMode): (Int, Int) = {
      val (mm, mmm) = mM
      val itvs = fm.checkIntervals.getOrElse(defaultIntervalChecks)
      (math.min(mm, itvs.min), math.max(mmm, itvs.max))
    }

    failures.foldLeft((1000000, 0))(minMax)
  }

  /** Generates definitions of min variables
   * Definitions for latent failures are omitted if there are no latent failures in the model.
   * */
  def genMinVariables(model: Model, udef: UserDefinedConstraints, result: MilpModel): Unit = {
    val (active, latent) = FailureMode.iterator.partition(_.active)
    val activeL = active.toList
    val latentL = latent.toList
    genMinVariablesGlobal(model, udef, result, activeL ++ latentL)
    genMinVariablesActive(model, result, activeL)
    if (latentL.nonEmpty)
      genMinVariablesLatent(model, udef, result, latentL)

  }

  /** Generates the budget constraint model */
  def genMilp(model: Model, udef: UserDefinedConstraints, indepRelation: Option[IndepRelation], criterion: BudgetCriterion, lambdaMapFileName: String, itvMapFileName: String): MilpModel = {
    // where result of translation is stored
    val result = new MilpModel
    // iterate on cuts
    for ((fc, fcUid) <- model.failureConditions.zipWithIndex;
         (mcs, mcsUid) <- fc.cuts.zipWithIndex) {
      // generate all configurations for this cut
      val cutSels = genCutSelectors(fc, fcUid, mcs, mcsUid, udef.aircraftData.get.checkIntervals)
      // generate upper and lower bounds constraints for cut selectors
      cutSels.foreach(cs => result.add(MilpLowerBoundConstraint(cs.milpVar, MilpIntLit(0))))
      cutSels.foreach(cs => result.add(MilpUpperBoundConstraint(cs.milpVar, MilpIntLit(1))))

      // declare all selector variables
      cutSels.foreach(cs => result.add(cs.milpVar))

      // at most one cut selector must be true
      result.add(MilpSpecialOrderedSet(cutSels.map(x => x.milpVar), 1))

      // at least one cut selector must be true
      result.add(MilpConstraint(lp.model.MilpSum(cutSels.map(_ => MilpIntLit(1)), cutSels.map(x => x.milpVar)), MilpGe, MilpIntLit(1)))

      // generate all implications between cut configurations and failure mode selectors
      cutSels.foreach(cs => result.add(genSelectorImplications(cs)))

      // instantiate latency limitation constraint for cuts that have latent failures
      if (!mcs.failureModes.exists(_.active)) {
        println("Warning: found a cut with no active failures: " + mcs.toString)
        //throw new Exception("Found a cut with no active failures: " + mcs.toString)
      }
      else {
        //result.add(genResidualRiskConstraint(mcs, udef.aircraftData.get.T0))
      }

      // instanciate constraints for each cut configuration
      for (cutSel <- cutSels) {
        val c = genBudgetConstraint(fc, cutSel, udef.aircraftData.get.T0)
        if (c.isDefined)
          result.add(c.get)
        // these constraints are only to be added for cuts that contain latent failures
        // if (!cutSel.mcs.failureModes.filter(x => x.latent).isEmpty) {
        //result.add(genLatencyLimitationConstraint(cutSel, udef.aircraftData.get.T0))
        // }
      }
    }

    // iterate on failure modes
    val lambdaMapFile = new BufferedWriter(new FileWriter(lambdaMapFileName))
    val itvMapFile = new BufferedWriter(new FileWriter(itvMapFileName))

    for (fm <- FailureMode.iterator) {
      lambdaMapFile.write(translator.LogLambdaFM(fm).toString)
      lambdaMapFile.write(" ")
      lambdaMapFile.write('\'')
      lambdaMapFile.write(fm.function.name.s)
      lambdaMapFile.write('.')
      lambdaMapFile.write(fm.name.s)
      lambdaMapFile.write('\'')
      lambdaMapFile.write("\n")
      // generate bounds on lambdas
      result.add(MilpUpperBoundConstraint(translator.LogLambdaFM(fm).milpVar, MilpRealLit(fm.uBound)))
      result.add(MilpLowerBoundConstraint(translator.LogLambdaFM(fm).milpVar, MilpRealLit(fm.lBound)))

      // selector constraints
      if (fm.latent) {
        val fmCheckIntervals = fm.checkIntervals.getOrElse(udef.aircraftData.get.checkIntervals)
        // generate all interval check selectors for this failure mode
        val selectors = fmCheckIntervals.map(itv => translator.FailureSelector(fm, itv).milpVar)

        // declare all variables
        selectors.foreach(s => result.add(s))

        // dump to map file
        val sels = fmCheckIntervals.map(itv => translator.FailureSelector(fm, itv))
        sels.foreach(s => {
          itvMapFile.write(s.toString)
          itvMapFile.write(" ")
          itvMapFile.write('\'')
          itvMapFile.write(s.fm.function.name.s)
          itvMapFile.write('.')
          itvMapFile.write(s.fm.name.s)
          itvMapFile.write('\'')
          itvMapFile.write(" ")
          itvMapFile.write(s.itv.toString)
          itvMapFile.write("\n")
        })

        // selectors are binary variables
        selectors.foreach(s => result.add(MilpLowerBoundConstraint(s, MilpIntLit(0))))
        selectors.foreach(s => result.add(MilpUpperBoundConstraint(s, MilpIntLit(1))))

        // at most one selector  true
        result.add(MilpSpecialOrderedSet(selectors, 1))

        // at least one selector true
        result.add(MilpConstraint(MilpSum(fmCheckIntervals.map(_ => MilpIntLit(1)), selectors), MilpGe, MilpIntLit(1)))

      } else {
        val selectors = List(translator.FailureSelector(fm, 1).milpVar)
        selectors.foreach(s => result.add(s))
        // at most one
        result.add(MilpConstraint(MilpSum(List(MilpIntLit(1)), selectors), MilpGe, MilpIntLit(1)))
        // at least one
        result.add(MilpConstraint(MilpSum(List(MilpIntLit(1)), selectors), MilpLe, MilpIntLit(1)))
      }
    }
    lambdaMapFile.close()
    itvMapFile.close()
    genMinVariables(model, udef, result)
    // encode max-min optimisation
    // TODO add minSumItvLatent
    val c = List(MilpRealLit(criterion.minLambdaGlobalW),
      MilpRealLit(criterion.minLambdaLatentW),
      MilpRealLit(criterion.minLambdaActiveW),
      MilpRealLit(criterion.minSumLambdaLatentW),
      MilpRealLit(criterion.minSumLambdaActiveW),
      MilpRealLit(criterion.minItvLatentW))
    val v = List(minLambdaGlobal,
      minLambdaLatent,
      minLambdaActive,
      minSumLambdaLatent,
      minSumLambdaActive,
      minItvLatent)
    result.criterion = MilpMax(MilpSum(c, v))
    result
  }
}
