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

package dalculator.cli

import dalculator.DalculatorCore
import dalculator.translator.ModelParser

object CLI {

  def printUsage(): Unit = {
    println("Usage: dalculator <param_file> ")
    println("-help : print this message and exit.")
  }


  def main(args: Array[String]): Unit = {
    val params = new DalculatorParameters

    // Decode command line arguments
    val argsList = args.toList

    for (opt <- argsList) {
      println("arg: " + opt)
      if (opt.startsWith("-help")) {
        printUsage()
        System.exit(1)
      } else {
        ModelParser.loadParamsFile(opt, params)
        // run specified commands
        DalculatorCore(params)
        System.exit(0)
      }
    }
  }
}
