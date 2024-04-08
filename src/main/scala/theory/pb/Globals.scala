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

package theory.pb

import dalculator.utils.FileManager
import theory.Logic

object Globals {

  /**
   * Path to the sat4j-pb solver.
   * */
  val sat4jPath: String =
    (for (inLib <- FileManager.libraryDirectory.locate("sat4j-pb.jar"))
      yield inLib.getAbsolutePath).get

  /**
   * Path to the wbo solver.
   * */
  val wboPath: String = (for (inLib <- FileManager.libraryDirectory.locate("wbo1.4b-fixed"))
    yield inLib.getAbsolutePath).get

//  /**
//   * Path to the pwbo solver.
//   * */
//  val pwboPath: String = rootPath + "/pwbo1.2"
//
//  /**
//   * Path to the clasp solver.
//   * */
//  val claspPath: String = rootPath + "/clasp"
//
//  /**
//   * Path to the minisat solver.
//   * */
//  val minisatPath: String = rootPath + "/minisat+_bignum_static"
//
//  /**
//   * Default timeout for the pwbo solver.
//   * */
//  val pwboTimeLimit = 43200

  /*
	 * Globally rename logical package imports to avoid name conflicts.
	 */
  type OpbLiteral = Int
  type OpbCoeff = Int
  type LModel = Logic.LogicModel
  type LSearch = Logic.Criterion
  type LSolution = Logic.LogicModelSat
  val LSolve: Logic.Criterion = Logic.Sat
  val LMinimize: Logic.Min.type = Logic.Min
  val LMaximize: Logic.Max.type = Logic.Max
}
