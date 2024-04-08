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

package preprocessor.translators

import preprocessor.analysis.AnalysisTypes.Real
import preprocessor.ast.{Always, Exponential, Law}

import scala.xml.Elem

/**
 * Created by kdelmas on 06/02/17.
 */
object DSLOpenPSATranslator {
  val treeDef = "define-fault-tree"
  val gateDef = "define-gate"
  val gateCall = "gate"
  val basicEventCall = "basic-event"
  val houseEventCall = "house-event"
  val basicEventDef = "define-basic-event"
  val houseEventDef = "define-house-event"
  val parameterCall = "parameter"
  val parameterDef = "define-parameter"
  val idKey = "name"

  private var gateNum = 0

  def reinit(): Unit = {
    gateNum = 0
  }

  def generateFreshGateName: String = {
    gateNum += 1
    s"gate$gateNum"
  }

  def createBasicEventDef(name: String, law: Elem): Elem = {
    <define-basic-event name={name}>
      {law}
    </define-basic-event>
  }

  def createBasicEventCall(name: String): Elem = {
      <basic-event name={name}/>
  }

  def createHouseEventDef(name: String, value: Elem): Elem = {
    <define-house-event name={name}>
      {value}
    </define-house-event>
  }

  def createHouseEventCall(name: String): Elem = {
      <house-event name={name}/>
  }

  def createConstantValue[T](value: T): Elem = {
    value match {
      case v: Int => <int value={v.toString}/>

      case v: Boolean => <constant value={v.toString}/>

      case v: Double => <float value={v.toString}/>

      case v: Real =>
        val n = implicitly[Numeric[Real]]
          <float value={n.toDouble(v).toString}/>
    }
  }

  def createMissionTime(value: Double): Elem = {
      <mission-time value={value.toString}/>
  }

  def createLaw(law: Law): Elem = {
    law match {
      case Always => createConstantValue(Real(1))
      case Exponential(l) =>
        <exponential>
          {createConstantValue(l)}<mission-time/>
        </exponential>
    }
  }

  def createAtLeast(k: Int, sons: List[Elem]): Elem = {
    sons match {
      case Nil => createConstantValue(false)
      case h :: Nil => h
      case _ =>
        <define-gate name={generateFreshGateName}>
          <atleast min={k.toString}>
            {sons}
          </atleast>
        </define-gate>
    }
  }

  def createNot(son: Elem): Elem = {
    <not>
      {son}
    </not>
  }

  def createGateCall(name: String): Elem = {
      <gate name={name}/>
  }

  def createParamDef(name: String, value: Elem): Elem = {
    <define-parameter name={name}>
      {value}
    </define-parameter>
  }

  def createParamCall(name: String): Elem = {
      <parameter name={name}/>
  }

  def createCall(e: Elem): Elem = {
    e.label match {
      case s if s == gateDef =>
        createGateCall(e.attribute(idKey).get.head.text)
      case s if s == parameterDef =>
        createParamCall(e.attribute(idKey).get.head.text)
      case s if s == basicEventDef =>
        createBasicEventCall(e.attribute(idKey).get.head.text)
      case s if s == houseEventDef =>
        createHouseEventCall(e.attribute(idKey).get.head.text)
      case s if s == gateCall | s == parameterCall | s == basicEventCall | s == houseEventCall => e
    }
  }
}
