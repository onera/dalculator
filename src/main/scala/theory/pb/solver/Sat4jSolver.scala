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

package theory.pb.solver

import theory.pb.Globals._

abstract class Sat4jSolver(cmd: String) extends OpbSolver(cmd) {
  override def commandLine(modelFile: String, globalTimeout: Long, resultTimeout: Long): (String, Long, Long) = {
    if (globalTimeout != -1)
      (cmd + " " + (globalTimeout / 1000).toString + """ """ + modelFile, -1, resultTimeout)
    else
      super.commandLine(modelFile, globalTimeout, resultTimeout)
  }
}

object Sat4jBoth extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ Both""")

object Sat4jMiniLearning extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ MiniLearningOPBClauseCardConstrMaxSpecificOrderIncrementalReductionToClause""")

object Sat4jMixed extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ PBCPMixedConstraintsObjectiveLearnJustClauses""")

object Sat4jCompet extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ CompetPBResMinHTMixedConstraintsObjectiveExpSimp""")

object Sat4jCP extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ CuttingPlanes""")

object Sat4jRes extends Sat4jSolver("""java -Xms256M -jar """ + sat4jPath + """ Resolution""")

//object WBOCores extends OpbSolver(wboPath + """ -file-format=opb -search-mode=1""")
//
//object WBOLinear extends OpbSolver(wboPath + """ -file-format=opb -search-mode=0""")
//
//object WBOLinearCores extends OpbSolver(wboPath + """ -file-format=opb -search-mode=2""")