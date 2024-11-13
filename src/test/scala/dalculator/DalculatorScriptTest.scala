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

import dalculator.cli.CLI
import dalculator.utils.FileManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DalculatorScriptTest extends AnyFlatSpec with should.Matchers {

  "For reduced FCS, the dalculator CLI" should "find a valid resource allocation" in {
    for {
      commandFile <- FileManager.extractResourceAsFile("fcs/commandFiles/analyse_port_colocation_nozones.txt")
    } yield {
      CLI.main(Array(commandFile))
    }
  }

  "For FAZER, the dalculator CLI" should "find a valid FDAL allocation" in {
    for {
      commandFile <- FileManager.extractResourceAsFile("FAZER/commandFiles/fdal_analysis.txt")
    } yield {
      CLI.main(Array(commandFile))
    }
  }

  "For FAZER, the dalculator CLI" should "find a valid IDAL allocation" in {
    for {
      commandFile <- FileManager.extractResourceAsFile("FAZER/commandFiles/idal_analysis.txt")
    } yield {
      CLI.main(Array(commandFile))
    }
  }

}
