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

package preprocessor.analysis

import dalculator.model.{AllocCstr, DalLevel, FailureMode, IndepRelation, Item, UserDefinedConstraint}
import preprocessor.ast._

import java.io.File
import scala.collection.immutable.SortedSet

object AnalysisTypes {

  abstract class AnalysisFile(path:String, val extension:String) extends File(AnalysisFile.normalize(path,extension)){

    def getBaseName:String = getName.split(".") match {
      case Array() => ""
      case a => a.head
    }
  }
  case class OCASFile(path:String) extends AnalysisFile(path, ".seq")
  case class XFTAFile(path:String) extends AnalysisFile(path,".xml")
  case class IDPFile(path:String) extends AnalysisFile(path, "idp")
  case class ResultFile(path:String) extends AnalysisFile(path,".txt")
  case class UDEFFile(path:String) extends AnalysisFile(path, ".udef")

  type FullXFTAFile = XFTAFile with FCField

  object FullXFTAFile {
    def apply(path:String, fc2:Map[FC,String]): FullXFTAFile = new XFTAFile(path) with FCField {
      val fc: Map[FC, String] = fc2
    }
  }

  trait FCField {
    val fc: Map[FC,String]
  }

  object AnalysisFile {
    def normalize(name:String, extension:String):String = {
      val purged = name.replace(" ","_")
      if(purged.contains(extension))
        purged
      else
        purged + extension
    }
  }


  type Real = Double

  object Real {
    def apply(d: Double): Real = d

    def apply(s:String): Real = s.toDouble
  }

  type CS = SortedSet[FailureMode]
  type MCS = SortedSet[CS]
  type MCSAnalysis = Analysis[Map[FC,MCS]]

  object MCSAnalysis {
    def apply(fc:FC, x : MCS): MCSAnalysis = Analysis(Map(fc -> x))
    def apply(x : Map[FC,MCS]): MCSAnalysis = Analysis(x)
  }

  type MS = List[FailureMode]
  type MSS = (SortedSet[MS],MCS)
  type MSSAnalysis = Analysis[Map[FC,MSS]]

  object MSSAnalysis {
    def apply(fc:FC, x : MSS): MSSAnalysis = Analysis(Map(fc -> x))
    def apply(x : Map[FC,MSS]): MSSAnalysis = Analysis(x)
  }

  type FHA = Analysis[Map[FC,Sev]]

  object FHA {
    def apply(x : Map[FC,Sev]): FHA = Analysis(x)
  }

  type FMEA = Analysis[Map[FailureMode, FC]]

  object FMEA {
    def apply(x:Map[FailureMode,FC]): FMEA = Analysis(x)
  }

  type Prob = Analysis[Map[FailureMode, Law]]

  object Prob {
    def apply(x:Map[FailureMode, Law]): Prob = Analysis(x)
  }

  type DALAnalysis = Analysis[Map[Item, DalLevel]]

  object DALAnalysis {
    def apply(x:Map[Item, DalLevel]): DALAnalysis = Analysis(x)
  }

  type ResourceAnalysis = Analysis[Option[(IndepRelation, List[UserDefinedConstraint], List[AllocCstr])]]

  object ResourceAnalysis {
    def apply(x:Option[(IndepRelation, List[UserDefinedConstraint], List[AllocCstr])]): ResourceAnalysis = Analysis(x)
  }

  type OrderAnalysis = Analysis[Map[FC,Option[Int]]]

  object OrderAnalysis {
    def apply(x:Map[FC,Option[Int]]): OrderAnalysis = Analysis(x)
  }

  type ReliabilityAnalysis = Analysis[Map[FC, Real]]

  object ReliabilityAnalysis {
    def apply(x:Map[FC,Real]): ReliabilityAnalysis = Analysis(x)
  }

  case class TableLine(fc:FC, nbScenario:Int, scenarioSize:Int, scenarioExplanation:String) extends Ordered[TableLine]{
    override def toString: String = s"${fc.name.name},$scenarioSize,$nbScenario,$scenarioExplanation"

    override def compare(that: TableLine): Int =
      if(fc.name.name != that.fc.name.name)
        fc.name.name.compare(that.fc.name.name)
      else if(scenarioSize != that.scenarioSize)
        scenarioSize.compare(that.scenarioSize)
      else if (nbScenario != that.nbScenario)
        nbScenario.compare(that.nbScenario)
      else
        scenarioExplanation.compare(that.scenarioExplanation)
  }
  type CommentedTable = Iterable[TableLine]
  type TableAnalysis = Analysis[CommentedTable]

  object TableAnalysis {
    def apply(x:CommentedTable): TableAnalysis = Analysis(x)
  }

  sealed trait Tree[E]
  case class Gate[E](k:Int, sons:List[Tree[E]]) extends Tree[E]
  case class Leaf[E](e:E) extends Tree[E]
  case class Not[E](son:Tree[E]) extends Tree[E]

  type FT = Tree[FailureMode]
  type FTA = Analysis[Map[FC,FT]]

  object FTA {
    def apply(x:Map[FC,FT]): FTA = Analysis(x)
  }
}
