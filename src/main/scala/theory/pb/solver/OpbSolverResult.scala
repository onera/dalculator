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

/** A class for storing OPB solver results.
 *
 * @constructor
 * @param modelFile       The source file from which this result was obtained.
 * @param commandLine     The command line used to analyse this file.
 * @param returnValue     The return value of the command line.
 * @param instanceStatus  Instance Status (SATISFIABLE, UNSATISFIABLE, UNKOWN).
 * @param objectiveValues Objective function value(s).
 * @param modelValues     Solution (if the instance is SAT).
 * */
case class OpbSolverResult(
                            modelFile: String,
                            commandLine: String,
                            returnValue: Option[Int] = None,
                            duration: Long = -1,
                            instanceStatus: Option[String] = None,
                            objectiveValues: Option[List[(String, Long)]] = None,
                            modelValues: Option[List[String]] = None)
