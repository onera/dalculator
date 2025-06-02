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

package dalculator.gui


/**
 * @author Rémi Delmas
 * */

import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.PrintStream
import javax.swing.SwingWorker
import theory.pb.solver._
import dalculator.cli._
import dalculator.model.DalLevel._
import dalculator.model._

import scala.language.postfixOps

class DalculatorGuiWithListener extends DalculatorGui with ActionListener {

  // worker thread in which the analysis is run
  private var analysisThread: Option[SwingWorker[Unit, Unit]] = None

  // install all commands and listeners
  // Inputs
  this.addSeqFile.setActionCommand("addSeqFile")
  this.addSeqFile.addActionListener(
    new AddSeqFileListener(
      this,
      this.seqTable.getModel.asInstanceOf[SeqFileTableModel]
    ))
  this.removeSeqFile.setActionCommand("removeSeqFile")
  this.removeSeqFile.addActionListener(new RemoveSeqFileListener(this.seqTable))

  this.runAnalysis.setActionCommand("runAnalysis")
  this.runAnalysis.addActionListener(this)

  this.cancelAnalysis.setActionCommand("cancelAnalysis")
  this.cancelAnalysis.addActionListener(this)

  // Indep panel
  this.selectIndepUdefFile.setActionCommand("selectIndepUdefFile")
  this.selectIndepUdefFile.addActionListener(new SelectUdefActionListener(this, this.indepUdefFile))
  this.selectIndepResultsFile.setActionCommand("selectIndepResultsFile")
  this.selectIndepResultsFile.addActionListener(new SelectUdefActionListener(this, this.indepResultsFile))
  this.indepSAT4J.setActionCommand("sat4jboth")
  this.indepSAT4J.setSelected(true)
  this.indepWBO.setActionCommand("wbo")


  // DAL panel
  this.selectDalUdefFile.setActionCommand("selectDalUdefFile")
  this.selectDalUdefFile.addActionListener(new SelectUdefActionListener(this, this.dalUdefFile))
  this.selectDalResultsFile.setActionCommand("selectDalResultsFile")
  this.selectDalResultsFile.addActionListener(new SelectUdefActionListener(this, this.dalResultsFile))
  this.dalSAT4J.setActionCommand("sat4jboth")
  this.dalSAT4J.setSelected(true)
  this.dalWBO.setActionCommand("wbo")
  this.dalRule1.setActionCommand("rule1")
  this.dalRule1.setSelected(true)
  this.dalRule2.setActionCommand("rule2")
  this.dalRuleMixed.setActionCommand("combined")

  // Budget panel
  this.selectBudgetResultsFile.setActionCommand("selectBudgetResultsFile")
  this.selectBudgetResultsFile.addActionListener(new SelectOutputFileActionListener(this, this.budgetResultsFile))
  this.selectBudgetUdefFile.setActionCommand("selectBudgetUdefFile")
  this.selectBudgetUdefFile.addActionListener(new SelectUdefActionListener(this, this.budgetUdefFile))

  // redirection of stdout and stderr to GUI
  private val console = new PrintStream(new JTextAreaOutputStream(this consoleTextArea))
  System.setOut(console)
  System.setErr(console)

  def actionPerformed(e: ActionEvent): Unit = e.getActionCommand match {

    case "runAnalysis" =>
      if (analysisThread.isEmpty) {
        println("Gathering parameters")

        // generate data structure containing arguments for analysis
        val params = new DalculatorParameters

        // Gather all input files, severities and budgets.
        val tableModel = this.seqTable.getModel
        val rowCount = tableModel.getRowCount
        params.seqFiles = Range(0, rowCount).map(i => tableModel.getValueAt(i, 0).asInstanceOf[String]).toList
        params.nSevs = Range(0, rowCount).map(i => tableModel.getValueAt(i, 1).asInstanceOf[Int]).toList
        params.xSevs = Range(0, rowCount).map(i => BigDecimal(tableModel.getValueAt(i, 2).asInstanceOf[Double])).toList
        params.refDals = Range(0, rowCount).map(
          i => tableModel.getValueAt(i, 3).asInstanceOf[String] match {
            case "A" => DalA
            case "B" => DalB
            case "C" => DalC
            case "D" => DalD
            case "E" => DalD
          }
        ).toList
        println(params.refDals)

        // Gather Indep parameters
        if (this.enableIndep.getModel.isSelected) {
          params.indepEnabled = true
          params.indepSaveAlloc = this.indepSaveAlloc.getModel.isSelected
          if (this.indepUdefFile.getText() != "")
            params.indepUdefFile = Some(this.indepUdefFile.getText())

          if (this.indepResultsFile.getText() != "") {
            params.indepResultsFile = this.indepResultsFile.getText()
          }

          params.indepSolver = this.indepSolverGroup.getSelection.getActionCommand match {
            case "sat4jboth" => Sat4jBoth
//            case "wbo" => WBOCores
          }

          try {
            params.indepResourcesStart = this.nofResourcesStart.getText().toInt
          } catch {
            case _: Exception => println("Wrong format for number of resources start")
          }

          try {
            params.indepResourcesStop = this.nofResourcesStop.getText().toInt
          } catch {
            case _: Exception => println("Wrong format for number of resources stop")
          }
        }
        // Gather Dal parameters
        if (this.enableDal.getModel.isSelected) {
          params.dalEnabled = true
          params.dalImportIndep = this.dalImportIndep.getModel.isSelected

          if (this.dalUdefFile.getText() != "")
            params.dalUdefFile = List(this.dalUdefFile.getText())

          if (this.dalResultsFile.getText() != "") {
            params.dalResultsFile = this.dalResultsFile.getText()
          }

          params.dalSolver = this.dalSolverGroup.getSelection.getActionCommand match {
            case "sat4jboth" => Sat4jBoth
//            case "wbo" => WBOCores
          }

          params.dalRule = this.dalRuleGroup.getSelection.getActionCommand match {
            case "rule1" => DalRule1
            case "rule2" => DalRule2
            case "combined" => DalRuleCombined
          }
        }
        // Gather budget parameters
        if (this.enableBudget.getModel.isSelected) {
          params.budgetEnabled = true
          params.budgetImportIndep = this.budgetImportIndep.getModel.isSelected

          if (this.budgetUdefFile.getText() != "")
            params.budgetUdefFile = Some(this.budgetUdefFile.getText())

          if (this.indepResultsFile.getText() != "") {
            params.budgetResultsFile = this.budgetResultsFile.getText()
          }

          // Criterion Parameters
          val c1: BigDecimal = if (this.enableMinLatent.getModel.isSelected) 0.2 else 0.0
          val c2: BigDecimal = if (this.enableMinActive.getModel.isSelected) 0.2 else 0.0
          val c3: BigDecimal = if (this.enableSumLatent.getModel.isSelected) 0.2 else 0.0
          val c4: BigDecimal = if (this.enableSumActive.getModel.isSelected) 0.2 else 0.0
          val c5: BigDecimal = if (this.enableMinCheckItv.getModel.isSelected) 0.2 else 0.0
          val c6: BigDecimal = if (this.enableGlobalLambdaMin.getModel.isSelected) 0.2 else 0.0
          params.budgetCriterion = BudgetCriterion(c6, c1, c2, c3, c4, c5)
        }

        // create a swing worker thread
        val frame = this
        val analysis = new SwingWorker[Unit, Unit]() {
          def doInBackground(): Unit = {
            frame.analysisThread = Some(this)
            println("analysis is running")
            dalculator.DalculatorCore(params)
          }

          override def done(): Unit = {
            println("analysis is done")
            frame.analysisThread = None
          }
        }
        analysis.execute()
      } else {
        println("ANALYSIS ALREADY RUNNING")
      }

    case "cancelAnalysis" =>
      // interrupt worker thread if it exists
      println("CANCEL NOT SUPPORTED YET")
    case x@_ => println(x)
  }
}

