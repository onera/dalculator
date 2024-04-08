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

package theory

import dalculator.model.DalLevel
import theory.pb.solver.{OpbSolver, OpbTrivialUnsat, Sat4jBoth}
import theory.pb.translator.Translator

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Classes to represent sets of quantifier free boolean/integer constraints.
 * */
object Logic {

  /** A pretty printer for logic syntax trees. */
  object PrettyPrint {
    def apply(e: Exp): String = e.toString // TODO
  }

  /** Base class of all expressions. */
  sealed abstract class Exp

  /** Base class of all Boolean expressions. */
  abstract class BoolExp extends Exp {
    def &(other: BoolExp): BoolExp = And(this, other)

    def |(other: BoolExp): BoolExp = Or(this, other)

    def <->(other: BoolExp): BoolExp = Iff(this, other)

    def ->(other: BoolExp): BoolExp = Implies(this, other)

    def unary_~ : BoolExp = Not(this)
  }

  /** Base class of all integer expressions. */
  abstract class IntExp extends Exp {
    def +(other: IntExp): Add = Add(this, other)

    def *(other: IntExp): Mult = Mult(this, other)

    def Ge(other: IntExp): LeInt = LeInt(other, this)

    def Gt(other: IntExp): LeInt = LeInt(Add(other, IntValue(1)), this)

    def Eq(other: IntExp): EqInt = EqInt(this, other)

    def Le(other: IntExp): LeInt = LeInt(this, other)

    def Lt(other: IntExp): LeInt = LeInt(Add(this, IntValue(1)), other)
  }

  /** Unary logical negation. */
  case class Not(kid1: BoolExp) extends BoolExp

  /** Logical n-ary conjunction. */
  case class And(kid1: BoolExp, kid2: BoolExp) extends BoolExp

  /** Logical n-ary disjunction. */
  case class Or(kid1: BoolExp, kid2: BoolExp) extends BoolExp

  /** Logical conditional choice. */
  case class IfBool(cond: BoolExp, t: BoolExp, e: BoolExp) extends BoolExp

  /** Logical identifier. */
  case class Ident(name: String) extends BoolExp

  sealed abstract class BoolValue extends BoolExp

  /** Logical truth. */
  case object True extends BoolValue

  /** Logical falsity. */
  case object False extends BoolValue

  /** Base class for relational expressions over integers. */
  abstract class RelExp extends BoolExp

  /** Less-than-or-equal n-ary relation over integers. */
  case class LeInt(kid1: IntExp, kid2: IntExp) extends RelExp

  /** Equal n-ary relation over integers. */
  case class EqInt(kid1: IntExp, kid2: IntExp) extends RelExp

  /** Counts the number of true expressions in the given population. */
  case class PopCount(pop: List[BoolExp]) extends IntExp

  /** Addition. */
  case class Add(kid1: IntExp, kid2: IntExp) extends IntExp

  /** Multiplication. */
  case class Mult(kid1: IntExp, kid2: IntExp) extends IntExp

  /** Base class for nested if-then-else constructs. */
  abstract class IfIntOrIntValue extends IntExp

  /** Conditional selection between two integer constants. */
  case class IfInt(cond: BoolExp, t: IfIntOrIntValue, e: IfIntOrIntValue) extends IfIntOrIntValue

  /** An integer constant. */
  case class IntValue(v: Int) extends IfIntOrIntValue

  trait OptHelper {
    def subjectTo(constraints: List[BoolExp]): LogicModel

    def subjectTo(constraints: BoolExp*): LogicModel
  }


  /** Implicit conversion from string to logical identifier. */
  implicit def identFromString(s: String): Ident = Ident(s)

  /** Implicit conversion from Int to IntValue. */
  implicit def intValueFromInt(v: Int): IntValue = IntValue(v)

  /** Implicit conversion from DAL level to IntValue. */
  implicit def intValueFromDALLevel(v: DalLevel): IntValue = IntValue(v.intValue)

  /** Population count. */
  def PopCount(pop: BoolExp*): PopCount = PopCount(pop.toList)

  /** n-ary conjunction. */
  def Add(pop: IntExp*): IntExp = Add(pop.toList)

  /** n-ary conjunction. */
  def Add(pop: List[IntExp]): IntExp = pop match {
    case Nil => IntValue(0)
    case h :: Nil => h
    case h :: t => t.foldLeft(h) { (res, x) => Add(res, x) }
  }

  /** n-ary conjunction. */
  def And(pop: BoolExp*): BoolExp = And(pop.toList)

  /** n-ary conjunction. */
  def And(pop: List[BoolExp]): BoolExp = pop match {
    case Nil => True
    case h :: Nil => h
    case h :: t => t.foldLeft(h) { (res, x) => And(res, x) }
  }

  /** n-ary disjunction. */
  def Or(pop: BoolExp*): BoolExp = Or(pop.toList)

  /** n-ary disjunction. */
  def Or(pop: List[BoolExp]): BoolExp = pop match {
    case Nil => False
    case h :: Nil => h
    case h :: t => t.foldLeft(h) { (res, x) => Or(res, x) }
  }

  /** Logical implication. */
  def Implies(e1: BoolExp, e2: BoolExp): Or = Or(Not(e1), e2)

  /** Logical equivalence. */
  def Iff(e1: BoolExp, e2: BoolExp): And = And(Implies(e1, e2), Implies(e2, e1))

  /** DSL construct '''If (cond) Then e1 Else e2'''. */
  def If(cond: BoolExp) = new Object {
    def Then(t: BoolExp) = new Object {
      def Else(e: BoolExp) = IfBool(cond, t, e)
    }

    def Then(t: IfIntOrIntValue) = new Object {
      def Else(e: IfIntOrIntValue) = IfInt(cond, t, e)
    }
  }

  /** Abstract class for criterion. */
  sealed abstract class Criterion

  /** Asks for a satisfying assignment. */
  case object Sat extends Criterion

  /** Asks for a criterion maximization. */
  case class Max(e: IntExp) extends Criterion

  /** Asks for a criterion minimization. */
  case class Min(e: IntExp) extends Criterion

  /** Base class of possible analysis results. */
  abstract class LogicModelStatus

  /** The model is sat and the map contains the values */
  case class LogicModelSat(values: mutable.HashMap[Ident, Boolean]) extends LogicModelStatus

  /** The model has an unknown status. */
  case object LogicModelUnsat extends LogicModelStatus

  /** The model is unsat. */
  case object LogicModelUnknown extends LogicModelStatus

  /** Boolean constraint system with optimisation criterion */
  case class LogicModel(criteria: List[Criterion], constraints: List[BoolExp]) {

    /** Solves the model using the given solver and given tmp file */
    def solve(solver: OpbSolver = Sat4jBoth, globalTimeout: Long = 120000, resultTimeout: Long = -1): LogicModelStatus = {
      try {

        val translator = new Translator(this)
        val (opbModel, btrFactory) = translator.get
        val opbResult = opbModel.solve(solver, globalTimeout, resultTimeout)
        val result = opbResult.instanceStatus match {
          case Some("UNSATISFIABLE") =>
            LogicModelUnsat
          case Some("SATISFIABLE") | Some("OPTIMUM FOUND") =>
            val btr = btrFactory.get(opbResult.modelValues.get)
            LogicModelSat(btr.identValues.map { x => {
              val (id, v) = x
              (id, v == True)
            }
            })
          case _ =>
            LogicModelUnknown
        }
        result
      } catch {
        case OpbTrivialUnsat => LogicModelUnsat
      }
    }
  }

  /** DSL construct for creating systems using : '''Satisfy(constraints)''' */
  def Satisfy(constraints: List[BoolExp]): LogicModel = LogicModel(List(Sat), constraints)

  /** DSL construct for creating systems using : '''Satisfy(cst1, ..., cst2)''' */
  def Satisfy(constraints: BoolExp*): LogicModel = LogicModel(List(Sat), constraints.toList)

  /** DSL construct for creating systems using : '''Maximize(e) subjectTo constraints''' */
  def Maximize(e: IntExp): OptHelper = new OptHelper {
    def subjectTo(constraints: List[BoolExp]): LogicModel = LogicModel(List(Max(e)), constraints)

    def subjectTo(constraints: BoolExp*): LogicModel = LogicModel(List(Max(e)), constraints.toList)
  }

  /** DSL construct for creating systems using : '''Minimize(e) subjectTo constraints''' */
  def Minimize(e: IntExp): OptHelper = new OptHelper {
    def subjectTo(constraints: List[BoolExp]): LogicModel = LogicModel(List(Min(e)), constraints)

    def subjectTo(constraints: BoolExp*): LogicModel = LogicModel(List(Min(e)), constraints.toList)
  }
}
