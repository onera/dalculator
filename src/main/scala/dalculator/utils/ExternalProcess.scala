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

package dalculator.utils

/** A class to execute external processes */
object ExternalProcess {

  /** calls the readOutput function on each line of the process invoked with the 'command' output, returns the return code of the output. */
  def exec(command: String)(readOutput: String => Unit): Int = {
    // build new process
    val proc = new ProcessBuilder(command.split(" "): _*).redirectErrorStream(true).start()
    // fetch output stream
    val procOut = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream))
    // create a thread to consume the output
    val gobbler = new Thread(new Runnable() {
      def run(): Unit = {
        var ln: String = null
        while ( {
          ln = procOut.readLine;
          ln != null
        }) readOutput(ln)
      }
    })
    gobbler.start()
    // TODO : verifier si le thread en attente a été interrompu, tuer le processus proc le cas échéant.
    proc.waitFor
    gobbler.join()
    procOut.close()
    proc.exitValue
  }

  /** returns a pair formed of the exit code of the process and the list of stdout prints (terminated with a newline) */
  def execl(cmd: String): (Int, List[String]) = {
    var res: List[String] = Nil
    val procOut = exec(cmd) { l => res = l :: res }
    (procOut, res)
  }
}
