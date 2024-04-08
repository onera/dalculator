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

package preprocessor.composer

import scala.collection.mutable
import preprocessor.analysis.AnalysisTypes._
import preprocessor.ast.FC
import preprocessor.translators.DSLOpenPSATranslator._

import scala.xml.Elem
import scala.xml.dtd.DocType

trait XFTAFileComposer[Pre] extends Composer[Pre] {
  type Result = XFTAFile
}
trait XFTAFCFieldFileComposer[Pre] extends Composer[Pre] {
  type Result = FullXFTAFile
}


trait XFTAFileInstances {

  implicit def FTAToXFTAComposer(implicit namer: Namer[FTA]) : XFTAFCFieldFileComposer[(FTA,Prob)] = new XFTAFCFieldFileComposer[(FTA,Prob)] {
    private val hasBeenDefined = mutable.Map[FT, Elem]()
    private val sons = mutable.ListBuffer[Elem]()

    def FTToPSARec(x:(FT, Prob)): Elem = x._1 match {
      case Leaf(e) =>
        hasBeenDefined.getOrElseUpdate(x._1, {
          val gate  = createBasicEventDef(e.name.s,createLaw(x._2.result(e)))
          sons += gate
          createCall(gate)
        })
      case Gate(k, s) =>
        val gDef = hasBeenDefined.getOrElseUpdate(x._1, {
          val r = createAtLeast(k, s.map(e => FTToPSARec((e,x._2))))
          sons += r
          r
        })
        createCall(gDef)
    }

    private def mkFCName(fc:FC):String = s"FC_${fc.name.name}"

    def FTAToPSA(x:(FTA, Prob)): Elem = {

      reinit()
      hasBeenDefined.clear()
      sons.clear()
      val tops = x._1.result.map(kv => {
        <define-gate name={mkFCName(kv._1)}>
          {FTToPSARec((kv._2,x._2))}
        </define-gate>
      })
      <open-psa>
        {sons}{tops}
      </open-psa>
    }

    def apply(pre: (FTA, Prob)): FullXFTAFile = {
      val result = FullXFTAFile(namer(pre._1), pre._1.result.map(kv => kv._1 -> mkFCName(kv._1)))
      scala.xml.XML.save(result.getAbsolutePath, FTAToPSA(pre), xmlDecl = true, doctype = DocType("open-psa"))
      result
    }
  }
}