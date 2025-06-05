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

package preprocessor.transformers

import dalculator.cli.DalculatorParameters
import dalculator.model.DalLevel.DalE
import dalculator.model.{AircraftData, AllocCstr, ColocCstr, Cost, DalCstr, DalLevel, DalLevelCstr, DalRelOp, DalRuleConstraint, FailureMode, IndepCstr, Item, LambdaBoundsCstr, LatentCstr, MaxCost, MinCost, Model, NoneIndepCsrt, NotIndepCstr, UserDefinedConstraints, UserDefinedOptimisation}
import dalculator.translator.ModelParser
import dalculator.utils.Configuration
import preprocessor.analysis.AnalysisTypes._
import preprocessor.ast.FC
import preprocessor.transformers.Parser._

import java.io.File

trait Parser[FT <: AnalysisFile] {
  type Result

  def parse(file: FT)(implicit conf:Configuration): Result
}

object Parser {

  type Aux[T <: AnalysisFile, R] = Parser[T] {
    type Result = R
  }

  sealed trait FCType

  case object MSSType extends FCType

  case object MCSType extends FCType
}

trait ParserInstances {

  implicit object OCASParser extends Parser[OCASFile] {
    type Result = Option[(FC, List[List[FailureMode]])]

    def parse(file: OCASFile)(implicit conf:Configuration): Option[(FC, List[List[FailureMode]])] = {
      val fm = new Model
      ModelParser.loadFC(file.path, fm, 2, -5, DalE)
      for {h <- fm.failureConditions.headOption} yield {
        FC(Symbol(h.name)) -> h.cuts.map(_.failureModes)
      }
    }
  }

  implicit object UDEFParser extends Parser[UDEFFile] {
    type Result = Map[Item,DalLevel]

    def parse(file: UDEFFile)(implicit conf:Configuration): Map[Item,DalLevel] = {
      val constraints = new UserDefinedConstraints
      ModelParser.loadUDef(file.path, constraints)
      constraints.dalConstraints.collect {
          case DalLevelCstr(item, DalRelOp.Eq, level) => item -> level
        }.toMap
    }
  }

}
