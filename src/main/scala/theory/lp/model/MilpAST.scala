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

package theory.lp.model

import scala.collection.mutable

/** Base class for nilp abstract syntax trees */
sealed abstract class MilpAST

/** Base class for milp variables */
abstract class MilpVar(name: String) extends MilpAST {
  override def toString: String = name
}

/** A real valued variable */
case class MilpRealVar(name: String) extends MilpVar(name)

/** An integer valued variable */
case class MilpIntVar(name: String) extends MilpVar(name)

/** A 0/1 valued variable */
case class MilpBinVar(name: String) extends MilpVar(name)

/** Lower bound of all lambdas */
case object minLambdaGlobal extends MilpVar("minLambdaGlobal")

/** Lower bound of lambdas of all latent failures */
case object minLambdaLatent extends MilpVar("minLambdaLatent")

/** Lower bound of lambdas of all active failures */
case object minLambdaActive extends MilpVar("minLambdaActive")

/** Lower bound of the sum of lambdas of all latent failures */
case object minSumLambdaLatent extends MilpVar("minSumLambdaLatent")

/** Lower bound of sum of lambdas of all active failures */
case object minSumLambdaActive extends MilpVar("minSumLambdaActive")

/** Lower bound of interval checks of all latent failures */
case object minItvLatent extends MilpVar("minItvLatent")

/** Lower bound of the sum of interval checks of all latent failures */
case object minSumItvLatent extends MilpVar("minSumItvLatent")

/** Base class for literal values */
sealed abstract class MilpLit extends MilpAST

/** Real literal */
case class MilpRealLit(value: BigDecimal) extends MilpLit {
  override def toString: String = value.toString
}

/** Integer literal */
case class MilpIntLit(value: BigInt) extends MilpLit {
  override def toString: String = value.toString
}

/** Boolean literal */
case class MilpBoolLit(value: Boolean) extends MilpLit {
  override def toString: String = value.toString
}

/** Relational operator */
sealed abstract class MilpRelOp extends MilpAST

/** Less than or equal */
case object MilpLe extends MilpRelOp {
  override def toString = "<="
}

/** Strictly less than */
case object MilpLt extends MilpRelOp {
  override def toString = "<"
}

/** Equal */
case object MilpEq extends MilpRelOp {
  override def toString = "="
}

/** Greater than or equal */
case object MilpGe extends MilpRelOp {
  override def toString = ">="
}

/** Strictly greater than */
case object MilpGt extends MilpRelOp {
  override def toString = ">"
}

/** A linear combination of variables */
case class MilpSum(coeffs: List[MilpLit], vars: List[MilpVar]) extends MilpAST {
  require(coeffs.length == vars.length)

  override def toString: String = (coeffs zip vars).map(x => x._1.toString + " " + x._2.toString).mkString(" + ")
}

/** A constraint that represents that a given sum is in relation with a given literal according to some operator */
class MilpConstraint(val sum: MilpSum, val op: MilpRelOp, val k: MilpLit, val label: Option[String] = None) extends MilpAST {
  override def toString: String = sum.toString + " " + op.toString + " " + k.toString
}

/** MilpConstraint factory */
object MilpConstraint {
  def apply(sum: MilpSum, op: MilpRelOp, k: MilpLit, l: Option[String] = None): MilpConstraint = new MilpConstraint(sum, op, k, l)

  def unapply(c: MilpConstraint): Option[(MilpSum, MilpRelOp, MilpLit, Option[String])] = Some(c.sum, c.op, c.k, c.label)
}

/** A constraint (represents that a given sum is in relation with a given literal according to some operator) */
case class MilpConditionalConstraint(condition: MilpVar, condWeight: MilpLit, override val sum: MilpSum, override val op: MilpRelOp, override val k: MilpLit, override val label: Option[String] = None) extends MilpConstraint(sum, op, k, label) {
  override def toString: String = condWeight.toString + " " + condition.toString + " + " + sum.toString + " " + op.toString + " " + k.toString
}

/** Lower bound constraint for a variable */
case class MilpLowerBoundConstraint(v: MilpVar, bound: MilpLit) extends MilpConstraint(MilpSum(List(MilpIntLit(1)), List(v)), MilpGe, bound)

/** Upper bound constraint for a variable */
case class MilpUpperBoundConstraint(v: MilpVar, bound: MilpLit) extends MilpConstraint(MilpSum(List(MilpIntLit(1)), List(v)), MilpLe, bound)

/** Optimisation criterion */
sealed abstract class MilpCriterion extends MilpAST

/** Find a solution */
case object MilpSolve extends MilpCriterion

/** Minimise given sum */
case class MilpMin(sum: MilpSum) extends MilpCriterion

/** Maximise given sum */
case class MilpMax(sum: MilpSum) extends MilpCriterion

/** Represents a special ordered set in the lp_solve sense (at most 'order' consecutive variables can be non-zero in the ordered set). */
case class MilpSpecialOrderedSet(vars: List[MilpVar], order: Int) extends MilpAST

/** Represents an Milp */
class MilpModel extends MilpAST {

  /** Set of all variables used in the system's constraints. */
  val declarations: scala.collection.mutable.HashSet[MilpVar] = new mutable.HashSet()

  /** The criterion to optimise */
  var criterion: MilpCriterion = MilpSolve

  /** The constraints */
  var constraints: List[MilpConstraint] = Nil

  /** Set of mutually exclusive variables */
  var specialOrderedSets: List[MilpSpecialOrderedSet] = Nil

  /** Upper bounds on variables */
  var upperBounds: List[MilpUpperBoundConstraint] = Nil

  /** Lower bounds on variables */
  var lowerBounds: List[MilpLowerBoundConstraint] = Nil

  /** Adds an element to the model */
  def add(elem: MilpAST): MilpModel = {
    elem match {
      case c: MilpConstraint => this.constraints ::= c
      case s: MilpSpecialOrderedSet => this.specialOrderedSets ::= s
      case v: MilpVar => this.declarations.add(v)
      case _ => throw new Exception("cannot add this type of thing")
    }
    this
  }

  /** Adds a list of elements to the model */
  def add(elems: List[MilpAST]): MilpModel = {
    elems.foreach(e => this.add(e))
    this
  }
}

