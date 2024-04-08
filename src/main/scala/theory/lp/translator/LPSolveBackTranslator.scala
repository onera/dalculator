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
 * */
package theory.lp.translator

import java.io._
import scala.collection.mutable
import scala.io.Source


class LPSolveBackTranslator(val lambdaMapFileName: String, val itvMapFileName: String, val outFileName: String) {
  val df = new java.text.DecimalFormat("#0.0000000000000000")
  val lambdaMap = new mutable.HashMap[String, String]()
  val itvMap = new mutable.HashMap[String, (String, String)]()
  val outFile = new BufferedWriter(new FileWriter(outFileName))

  // load lambda map
  private val lambdaSource = Source.fromFile(lambdaMapFileName)
  for (l <- lambdaSource.getLines()) {
    val items = l.split(" ")
    require(items.length == 2)
    lambdaMap.update(items(0), items(1))
  }
  lambdaSource.close()


  // load interval check maps
  private val itvSource = Source.fromFile(itvMapFileName)
  for (l <- itvSource.getLines()) {
    val items = l.split(" ")
    require(items.length == 3)
    itvMap.update(items(0), (items(1), items(2)))
  }
  itvSource.close()

  // load model file
  def readline(l: String): Unit = {
    println(l)
    val items = l.split(" ").filter(_ != "")
    items.length match {
      case 2 =>
        val key = items(0)
        val value = items(1)
        val x = lambdaMap.get(key)
        if (x.isDefined) {
          outFile.write("lambda   ")
          outFile.write(x.get)
          outFile.write(" = ")
          // outFile.write(scala.math.pow(10, value.toDouble).toString)
          // outFile.write(" | ")
          outFile.write(df.format(scala.math.pow(10, value.toDouble)))
          outFile.write(" | 10^(")
          outFile.write(value)
          outFile.write(")\n")
        } else {
          val y = itvMap.get(key)
          if (y.isDefined) {
            if (items(1) == "1") {
              val fm = y.get._1
              val itv = y.get._2
              outFile.write("interval ")
              outFile.write(fm)
              outFile.write(" = ")
              outFile.write(itv)
              outFile.write("\n")
            }
          } else if (key.startsWith("min")) {
            outFile.write(items.mkString("", " ", "\n"))
          }
        }
      case 0 =>
      case _ => outFile.write(items.mkString("", " ", "\n"))
    }
  }

  def closeFile(): Unit = outFile.close()
}
