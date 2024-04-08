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

package theory.pb.model

import java.io.BufferedWriter

/**
 * Header of a pseudo-boolean model.
 *
 * @constructor
 * @param nbVars Number of variables in the model.
 * @param nbCtrs Number of constraints in the model.
 * */
case class OpbHeader(nbVars: Int, nbCtrs: Int) {
  /** Writes a textual representation of the header to the given 'out'buffer */
  def dump(out: BufferedWriter): Unit = {
    out.write("* #variable= %s #constraint= %s\n".format(nbVars, nbCtrs))
  }
}
