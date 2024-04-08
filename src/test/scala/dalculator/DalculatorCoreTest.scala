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
import dalculator.model.DalLevel._
import dalculator.model._
import dalculator.solver.SolveDal
import dalculator.translator.ModelParser
import dalculator.utils.FileManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import preprocessor.analysis.AnalysisTypes.{DALAnalysis, MCS, MCSAnalysis, OCASFile}
import preprocessor.ast.FC
import preprocessor.composer.all._
import preprocessor.transformers.all._

import java.io.{File, FileWriter}
import scala.io.Source

class DalculatorCoreTest extends AnyFlatSpec with should.Matchers {

  private def exportDalAnalysis(dalAnalysis: Ops[Seq[(Item, DalLevel)]], filename: String, udef:Option[UserDefinedConstraints] = None): Unit = {
    val writer = new FileWriter(FileManager.analysisDirectory.getFile(filename))
    writer.write(
      dalAnalysis
        .map((i: Item) => i.originalName)
        .map((s: Seq[(String, DalLevel)]) =>
          Ops(DALAnalysis(s.groupMapReduce(k => Item(k._1))(_._2)((l, r) => if (l < r) r else l)))
            .showAll
            .value)
        .value)
    for{u <- udef} yield {writer.write(s"\nCost = ${SolveDal.allocCost(dalAnalysis.value, u)}")}
    writer.close()
  }

  private def analyseFFS(mcs: Map[FC, MCS], outputFile:String, replaces: Map[Item, String] = Map.empty): Unit = {
    val fcs = mcs.keys.map(_.name.name).toList.sorted
    val functions = mcs.values.flatten.flatten.map(x =>
      replaces.getOrElse(x.function,x.function.originalName)).toList.distinct.sorted
    val bySize = mcs.transform((_,v) => v.groupBy(_.size))
    val orderedSizes = {
      for {
        (fc, sizes) <- bySize
      } yield {
        fc -> (1 to sizes.keys.max)
      }
    }.toList.sortBy(_._1.name.name)
    val contribution = {
      for{
        f <- functions
        (fc,v) <- bySize
        (size, mcs) <- v
      } yield {
        (f,fc.name.name,size) -> mcs.count(_.exists(x => replaces.getOrElse(x.function,x.function.originalName) == f))
      }
    }.toMap
    val writer = new FileWriter(FileManager.exportDirectory.getFile(outputFile))
    writer.write(s"Component & ${fcs.mkString("& ")} \\\\\n")
    writer.write(s" & ${orderedSizes.map(_._2.mkString(" & ")).mkString(" & ")} \\\\\n")
    for {f <- functions} {
      writer.write(s"$f & ")
      val completedSizes = for {
        (fc,range) <- orderedSizes
        k <- range
      } yield {
        contribution.getOrElse((f,fc.name.name,k), 0)
      }
      writer.write(s"${completedSizes.mkString(" & ")} \\\\\n")
    }
    writer.close()
  }

  "For ARP example limited to order 3, the dalculator" should "find a valid DAL allocation" in {
    for {
      arpLoss <- FileManager.extractResourceAsFile("arpModel/sequences/ARP_Loss_3.seq")
      arpUntimely <- FileManager.extractResourceAsFile("arpModel/sequences/ARP_Untimely_3.seq")
      aprCosts <- FileManager.extractResourceAsFile("arpModel/userConstraints/costs.udef")
      aprConstraints <- FileManager.extractResourceAsFile("arpModel/userConstraints/constraints.udef")
    } yield {
      val userDefinedConstraints = new UserDefinedConstraints()
      ModelParser.loadUDef(aprCosts, userDefinedConstraints)
      ModelParser.loadUDef(aprConstraints, userDefinedConstraints)
      val mcs = from(List(arpLoss, arpUntimely).map(OCASFile)).derive[MCSAnalysis].result
      analyseFFS(mcs,"arp_function_contribution.txt")

      for {rule <- Set(DalRule1, DalRule2, DalRuleCombined)} {
        val dalAnalysis = Everywhere.on(from(List(arpLoss, arpUntimely).map(OCASFile)).derive[MCSAnalysis])
          .map((mcs: MCSAnalysis) => (
            mcs,
            mcs.result.transform[(Int, DalLevel)]((fc, _) => (2, if (fc.name.name.contains("Loss")) DalB else DalA)),
            userDefinedConstraints,
            rule
          ).derive[DALAnalysis].result.toSeq)
        exportDalAnalysis(dalAnalysis, s"ARP_DAL_Allocation_3_rule_$rule.txt", Some(userDefinedConstraints))
      }
    }
  }

  "For ARP example limited to order 4, the dalculator" should "find a valid DAL allocation" in {
    for {
      arpLoss <- FileManager.extractResourceAsFile("arpModel/sequences/ARP_Loss_4.seq")
      arpUntimely <- FileManager.extractResourceAsFile("arpModel/sequences/ARP_Untimely_4.seq")
      aprCosts <- FileManager.extractResourceAsFile("arpModel/userConstraints/costs.udef")
      aprConstraints <- FileManager.extractResourceAsFile("arpModel/userConstraints/constraints.udef")
    } yield {
      val userDefinedConstraints = new UserDefinedConstraints()
      ModelParser.loadUDef(aprCosts, userDefinedConstraints)
      ModelParser.loadUDef(aprConstraints, userDefinedConstraints)
      val mcs = from(List(arpLoss, arpUntimely).map(OCASFile)).derive[MCSAnalysis].result
      analyseFFS(mcs, "arp_function_contribution.txt")

      val dalAnalysis = Everywhere.on(from(List(arpLoss, arpUntimely).map(OCASFile)).derive[MCSAnalysis])
        .map((mcs: MCSAnalysis) => (
          mcs,
          mcs.result.transform[(Int, DalLevel)]((fc, _) => (2, if (fc.name.name.contains("Loss")) DalB else DalA)),
          userDefinedConstraints
        ).derive[DALAnalysis].result.toSeq)
      exportDalAnalysis(dalAnalysis, "ARP_DAL_Allocation_4.txt", Some(userDefinedConstraints))
    }
  }
  
  "For data display the dalculator" should "find a valid DAL allocation with combined DAL downgrade rules" in {
    for {
      f <- List("Sat_Eq", "Sat_Eq_2DALinf", "Sat_Eq_2DALsup", "Sat_Eq_DALAInfDALB", "Sat_Eq_pascout", "Sat_Ge", "Sat_Lt")
      seq <- FileManager.extractResourceAsFile("dataDisplay/sequences/TestOptimBis.seq")
      udef <- FileManager.extractResourceAsFile(s"dataDisplay/userConstraints/$f.udef")
    } yield {
      val params = new DalculatorParameters()
      params.seqFiles ::= seq
      params.nSevs ::= 3
      params.xSevs ::= BigDecimal(-9.0)
      params.refDals ::= DalA
      params.indepEnabled = true
      params.dalEnabled = true
      params.dalRule = DalRuleCombined
      params.dalResultsFile = FileManager.analysisDirectory.getFile(s"${f}_result.udef").getAbsolutePath
      params.dalUdefFile ::= udef
      params.budgetEnabled = false
      try {
        DalculatorCore(params)
      } catch {
        case _: Exception =>
          fail("The result should be SAT")
      }
    }
  }

  "For data display the dalculator" should "find a valid DAL allocation with cost criterion using only rule 2 " in {
    for {
      seq <- FileManager.extractResourceAsFile("dataDisplay/sequences/DataDisplay_0_order4.seq")
      indepUdef <- FileManager.extractResourceAsFile(s"dataDisplay/userConstraints/DataDisplay_0_order4_indep.udef")
      dalUdef <- FileManager.extractResourceAsFile(s"dataDisplay/userConstraints/DataDisplay_0_order4_dal.udef")
    } yield {
      val params = new DalculatorParameters()
      params.seqFiles ::= seq
      params.nSevs ::= 2
      params.xSevs ::= BigDecimal(-7.0)
      params.refDals ::= DalB
      params.indepEnabled = false
      params.dalImportIndep = true
      params.indepUdefFile = Some(indepUdef)
      params.dalEnabled = true
      params.dalUdefFile ::= dalUdef
      params.dalResultsFile = FileManager.analysisDirectory.getFile("DataDisplay_0_order4_dal_result.udef").getAbsolutePath
      params.dalRule = DalRule2
      params.budgetEnabled = false
      DalculatorCore(params)
    }
  }

  "For data display the dalculator" should "find a valid DAL allocation with DAL downgrade only rule1 or only rule 2 or common cause rule 2" in {
    for{
      seq <- FileManager.extractResourceAsFile("dataDisplay/sequences/DataDisplay_0_order4.seq")
      indepUdef <- FileManager.extractResourceAsFile(s"dataDisplay/userConstraints/DataDisplay_0_order4_indep.udef")
    } yield {
      for {
        rule <- Set(DalRule1, DalRule2)
      } yield {
        val params = new DalculatorParameters()
        params.seqFiles ::= seq
        params.nSevs ::= 2
        params.xSevs ::= BigDecimal(-7.0)
        params.refDals ::= DalB
        params.indepEnabled = false
        params.dalImportIndep = true
        params.dalEnabled = true
        params.dalResultsFile = FileManager.analysisDirectory.getFile(s"Display_Alloc_Option${rule}.udef").getAbsolutePath
        params.dalRule = rule
        params.budgetEnabled = false
        DalculatorCore(params)
      }
      val params = new DalculatorParameters()
      params.seqFiles ::= seq
      params.nSevs ::= 2
      params.xSevs ::= BigDecimal(-7.0)
      params.refDals ::= DalB
      params.indepEnabled = false
      params.dalImportIndep = true
      params.indepUdefFile = Some(indepUdef)
      params.dalEnabled = true
      params.dalResultsFile = FileManager.analysisDirectory.getFile("Display_Coloc_Option2.udef").getAbsolutePath
      params.dalRule = DalRule2
      params.budgetEnabled = false
      DalculatorCore(params)
    }
  }

  "For data display the dalculator" should "not find a valid DAL allocation due to unachievable user constraints" in {
    for {
      f <- List("Unsat_Eq", "Unsat_Lt", "TestOptimbis")
      seq <- FileManager.extractResourceAsFile("dataDisplay/sequences/TestOptimBis.seq")
      udef <- FileManager.extractResourceAsFile(s"dataDisplay/userConstraints/$f.udef")
    } yield {
      val params = new DalculatorParameters()
      params.seqFiles ::= seq
      params.nSevs ::= 3
      params.xSevs ::= BigDecimal(-9.0)
      params.refDals ::= DalA
      params.indepEnabled = true
      params.dalEnabled = true
      params.dalRule = DalRuleCombined
      params.dalUdefFile ::= udef
      params.budgetEnabled = false
      try {
        DalculatorCore(params)
        fail("The result should be UNSAT")
      } catch {
        case _: Exception =>
          succeed
      }
    }
  }
}
