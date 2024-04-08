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

/**
 * @author Rémi Delmas
 *         date 2011/10/15
 * */
package theory.pb.solver

import dalculator.utils.GetTime

import scala.sys.process.{Process, ProcessIO}

/**
 * Interface to OPB solvers.
 *
 * @constructor
 * @param cmd The command line string used to launch the solver process (with arguments).
 *
 *            This class launches the solver command as an external process and monitors the process using several threads.
 *            It will parse the solver's output and optionally interrupt the process when either
 *            a global timeout has been reached or the time spent by the solver to find two consecutive solutions exceeds a given timeout.
 */
class OpbSolver(val cmd: String) extends GetTime {
  /** External process monitoring period (defines how often interruption conditions are checked by the monitoring thread). */
  val monitoringPeriod: Int = 1000
  /** Instance status as returned by the solver. */
  private var s: Option[String] = None
  /** List of objective function values found by the solver with their time stamp. */
  private var o: List[(String, Long)] = Nil
  /** List of strings describing the solution found by the solver. */
  private var v: List[String] = Nil
  /** False is the solver process is running, true when it has stopped running by itself or has been interrupted. */
  private var solverDone: Boolean = false
  /** Time when the last result was found. */
  private var lastResult: Long = -1

  /** This method must return the command line used for invocation of the solver on the given model file with given timeouts, and return new timeout values (some of which can become -1 if the solver itself handles timeouts). */
  def commandLine(modelFile: String, globalTimeout: Long, resultTimeout: Long): (String, Long, Long) = (cmd + " " + modelFile, globalTimeout, resultTimeout)

  /**
   * Runs the solver on the given file.
   *
   * @param modelFile     OPB file containing the model to be analyzed.
   * @param globalTimeout time after which the solver process will be interrupted with a signal 15.
   * @param resultTimeout time after which the solver process will be interrupted after it has found a first solution and before it finds a next solution.
   * @return an OpbSolverResult instance.
   * */
  def run(modelFile: String, globalTimeout: Long = -1, resultTimeout: Long = -1): OpbSolverResult = {
    // re-initialize state of the solver instance
    s = None
    o = Nil
    v = Nil
    solverDone = false
    lastResult = -1

    // build the external process
    val (command, gto, rto) = commandLine(modelFile, globalTimeout, resultTimeout)
    val builder = Process(command)
    // multithreaded IO handler used to read output streams of the solver process.
    val pio = new ProcessIO(
      // ignore input stream
      _ => (),
      // parse output stream
      pStdout => {
        // parse the lines of the stream
        scala.io.Source.fromInputStream(pStdout).getLines().foreach(readSolverOutput)
        // when the stream becomes empty, it means the solver is done
        solverDone = true
      },
      // ignore error stream
      _ => ()
    )
    // memorize start time and run process
    val startTime = getTime
    println("Launching solver with command line: '" + command + "'")
    try {
      val process = builder.run(pio)
      // inform user of the PID
      getPID(command) match {
        case Some(pid) => println("The PID of the solver process is " + pid)
        case None => println("Could not determine the PID of the solver process, it will be brutally destroyed.")
      }

      // scans the solver state at regular intervals, and decides on interruption.
      while (true) {
        Thread.sleep(monitoringPeriod)
        // check if process is done
        if (solverDone) {
          println("Solver process completed (%dms).".format(getTime - startTime))
          return OpbSolverResult(modelFile, cmd, Some(process.exitValue()), getTime - startTime, s, Some(o), Some(v))
        } else {
          // check if there is a global timeout
          if (gto > -1) {
            val timeLeft = gto - (getTime - startTime)
            if (timeLeft <= 0) {
              println("Global timeout reached, interrupting solver.")
              interrupt(process, command)
            }
          }
          // check if there is a timeout between results
          if (rto > -1 && lastResult > -1) {
            val timeLeft = rto - (getTime - lastResult)
            if (timeLeft <= 0) {
              // kill process
              println("Timeout between consecutive results reached, interrupting solver.")
              interrupt(process, command)
            }
          }
        }
      }
      OpbSolverResult(modelFile, cmd, Some(process.exitValue()), getTime - startTime, s, Some(o), Some(v))
    } catch {
      case e: Exception =>
        println("[warn] error when trying to execute process '" + command + "'" + " with message '" + e.toString + "'")
        OpbSolverResult(modelFile, command)
    }
  }

  /** Tries to find the pid of the process started with the given command-line string. */
  private def getPID(cmd: String): Option[String] = {
    var out: List[String] = Nil
    val processCmd = "ps -wweo pid,command"
    val pb = Process(processCmd)
    val pio = new ProcessIO(
      _ => (), // do nothing on input stream
      pStdout => {
        scala.io.Source.fromInputStream(pStdout).getLines().foreach(s => out ::= s)
      },
      _ => () // do nothing on error stream
    )
    // synchronize on the process
    try {
      pb.run(pio).exitValue()
      // look for the process in the list returned by ps.
      for (l <- out) {
        val line = l.trim
        val pid = line.split(" ")(0)
        if (line.endsWith(cmd)) {
          // found what we want
          return Some(pid)
        }
      }
      // we did not find anything
      None
    } catch {
      case e: Exception =>
        println("[warn] error when trying to execute process '" + processCmd + "'" + " with message '" + e.toString + "'")
        None
    }
  }


  /** Sends a signal 15 to the given PID. */
  private def kill(sig: Int, pid: String): Unit = {
    val pb = Process("kill -" + sig.toString + " " + pid)
    pb.!
  }

  /** Interrupts the given command with a kill -15 if its PID can be found, otherwise destroys the process brutally. */
  private def interrupt(process: Process, command: String): Unit = {
    getPID(command) match {
      case Some(pid) => kill(15, pid)
      case None =>
        println("!!! Could not find PID for clean exit, destroying process !!!")
        process.destroy()
        solverDone = true
    }
  }

  /** Reads the stdout of an standards compliant OPB solver. */
  private def readSolverOutput(l: String): Unit = {
    if (l.startsWith("c core") || l.startsWith("c lb") || l.startsWith("c ub")) {
      // lower and upper bounds found
      println(l.take(78) + "..")
    }

    if (l.startsWith("o")) {
      // solution found
      lastResult = getTime
      println(l.take(78) + "..")
      o ::= (l.drop(2), getTime)
    }

    if (l.startsWith("s")) {
      // status line
      println(l.take(78) + "..")
      s = Some(l.drop(2))
    }

    if (l.startsWith("v")) {
      // model found
      println(l.take(78) + "..")
      v ::= l.drop(2)
    }
  }
}



