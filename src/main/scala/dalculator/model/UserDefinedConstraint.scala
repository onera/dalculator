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

package dalculator.model

/** Base class for user defined constraints. */
sealed abstract class UserDefinedConstraint

/** Class describing the type of aircraft that is considered */
case class AircraftData(T0: Int, checkIntervals: List[Int]) extends UserDefinedConstraint {

  /** TODO require (this.verifyAssumptions)
   * Verify all assumptions that must hold on interval checks.
   * */
  private def verifyAssumptions = {
    true
  }

  override def toString = "AircraftProfile( " + T0 + ", " + checkIntervals.mkString("(", ", ", ")") + ")"
}

/** Default configuration for short range aircrafts */
object ShortRange extends AircraftData(1, List(1, 10, 50, 200))

/** Default configuration long range aircrafts */
object LongRange extends AircraftData(10, List(1, 5, 20))

/** A constraint imposing that functions are allocated on a same resource (and hence not independent). */
case class AllocCstr(functions: List[Item], resource: Int) extends UserDefinedConstraint with Iterable[Item] {
  def iterator = functions.iterator

  override def toString = "Alloc(" + functions.map(_.originalNameQuoted).mkString(", ") + ", " + resource + ")"
}

/** A constraint imposing that functions are allocated on a same resource (and hence all pairwise not independent). */
case class ColocCstr(functions: List[Item]) extends UserDefinedConstraint with Iterable[Item] {
  def iterator = functions.iterator

  override def toString = "Coloc" + functions.map(_.originalNameQuoted).mkString("(", ", ", ")")
}

/** A constraint imposing that functions are mutually independent. */
case class IndepCstr(functions: List[Item]) extends UserDefinedConstraint with Iterable[Item] {
  def iterator = functions.iterator

  override def toString = "Indep" + functions.map(_.originalNameQuoted).mkString("(", ", ", ")")
}

/** A constraint imposing that functions are mutually independent. */
case class NotIndepCstr(functions: List[Item]) extends UserDefinedConstraint with Iterable[Item] {
  def iterator = functions.iterator

  override def toString = "NotIndep" + functions.map(_.originalNameQuoted).mkString("(", ", ", ")")
}

case class NoneIndepCsrt(functions: List[Item]) extends UserDefinedConstraint with Iterable[Item] {
  def iterator: Iterator[Item] = functions.iterator

  override def toString = "NoneIndep" + functions.map(_.originalNameQuoted).mkString("(", ", ", ")")
}

object DalRelOp extends Enumeration {
  type DalRelOp = Value
  val Eq, Lt, Ge = Value
}

import dalculator.cli.DefaultParameters
import dalculator.model.DalRelOp._
import dalculator.model

import java.io.{BufferedWriter, FileWriter}


sealed abstract class DalCstr extends UserDefinedConstraint {
  val left: Item
  val relOp: DalRelOp
}

/** A constraint imposing an upper bound on the dal level of a function. */
case class DalLevelCstr(left: Item, relOp: DalRelOp, right: DalLevel) extends DalCstr {
  override def toString = "Dal(" + left.originalNameQuoted + " " + relOp + " " + right.toString + ")"
}

/** ***************************************************** */
/** ***************************************************** */

/** A constraint imposing an upper, lower or equivalent relation between the dal levels of two functions */
case class DalqIdentCstr(left: Item, relOp: DalRelOp, right: Item) extends DalCstr {
  override def toString = "Dal(" + left.originalNameQuoted + " " + relOp + " " + right.originalNameQuoted + ")"
}

/** A constraint imposing the cost of a function for a certain DAL */
case class Cost(item: Item, DAL: DalLevel, value: Int) extends UserDefinedConstraint {
  override def toString = "Cost(" + item.originalNameQuoted + ", " + DAL + ") = " + value
}

/** A constraint imposing that all costs have to be greater than a minimal cost */
case class MinCost(value: Int) extends UserDefinedConstraint {
  override def toString = "Cmin =" + value
}

/** A constraint imposing that all costs have to be lower than a maximal cost */
case class MaxCost(value: Int) extends UserDefinedConstraint {
  override def toString = "Cmax = " + value
}

/** ***************************************************** */
/** ***************************************************** */

/** A constraint specifying that a FailureMode is latent,
 * and optinally specifying a set of possible check interval values.
 * */
case class LatentCstr(fm: FailureMode, itvs: Option[List[Int]] = None) extends UserDefinedConstraint {
  override def toString = "Latent( " + fm.originalNameQuoted + (if (itvs.isDefined) ", " + itvs.get.mkString("(", ", ", ")") else "") + ")"
}

/** A constraint specifying that the log of the lambda of a FailureMode shall be within
 * some user defined bounds.
 * */
case class LambdaBoundsCstr(
                             fm: FailureMode,

                             /** < A failure model */
                             lBound: BigDecimal = DefaultParameters.lambdaLowerBound,

                             /** < Lower bound of log(lambda(fm)) */
                             uBound: BigDecimal = DefaultParameters.lambdaUpperBound)

/** < Upper bound of log(lambda(fm)) */
  extends UserDefinedConstraint {
  override def toString = "LambdaBounds(" + fm.originalNameQuoted + ", " + uBound.toString + "," + lBound.toString + ")"
}

/** Represents the dal allocation rule to use. */
case class DalRuleConstraint(rule: DalRule) extends UserDefinedConstraint {
  override def toString = "DalRule(" + rule.toString + ")"
}


/** Abstract base class for optimisation criterions. */
abstract class UserDefinedCriterion

/** Cardinality of functions with a certain DAL level. */
case class DalCard(dalLevel: DalLevel) extends UserDefinedCriterion {
  override def toString = "DalCard " + dalLevel.toString
}

/** ******************************* */

/** Criterion of cost functions with a certain DAL level */
case object CostCriterion extends UserDefinedCriterion {
  override def toString = "Cost"
}

/** ****************************** */


/** Cardinality of the function independence relation. */
case object IndepCard extends UserDefinedCriterion {
  override def toString = "IndepCard"
}

/** Cardinality of the set of resources used by the allocation. */
case object ResourceCard extends UserDefinedCriterion {
  override def toString = "ResourceCard"
}

/** An optimisation command, applied to a criterion. */
abstract class UserDefinedOptimisation(val criterion: UserDefinedCriterion) extends UserDefinedConstraint

/** Requires Minimisation. */
case class OptMin(override val criterion: UserDefinedCriterion) extends UserDefinedOptimisation(criterion) {
  override def toString = "OptMin(" + criterion.toString + ")"
}

/** Requires Maximisation. */
case class OptMax(override val criterion: UserDefinedCriterion) extends UserDefinedOptimisation(criterion) {
  override def toString = "OptMax(" + criterion.toString + ")"
}

/** A class for storing user defined constraints and intermediary
 * results of analyses.
 * Remark : The 'latent' qualifier is represented as a user defined constraint,
 * but it is really an attribute of a failure mode,
 * independent of the will of the user.
 * */
class UserDefinedConstraints extends Iterable[UserDefinedConstraint] {
  var aircraftData: Option[AircraftData] = None
  var allocConstraints: List[AllocCstr] = Nil
  var colocConstraints: List[ColocCstr] = Nil
  var indepConstraints: List[IndepCstr] = Nil
  var notIndepConstraints: List[NotIndepCstr] = Nil
  var dalConstraints: List[DalCstr] = Nil
  var latentConstraints: List[LatentCstr] = Nil
  var boundsConstraints: List[LambdaBoundsCstr] = Nil
  var dalRule: DalRuleConstraint = DalRuleConstraint(DalRuleCombined)
  var optCriteria: List[UserDefinedOptimisation] = Nil
  var costConstraints: List[Cost] = Nil
  var minCostConstraints: List[MinCost] = Nil
  var maxCostConstraints: List[MaxCost] = Nil

  /** Adds a user defined constraints to this object */
  def add(c: UserDefinedConstraint): UserDefinedConstraints = {
    c match {
      case c: MaxCost =>
        this.maxCostConstraints ::= c
      case c: MinCost =>
        this.minCostConstraints ::= c
      case c: Cost =>
        this.costConstraints ::= c
      case c: AllocCstr => if(c.functions.nonEmpty) this.allocConstraints ::= c
      case c: ColocCstr => if(c.functions.nonEmpty) this.colocConstraints ::= c
      case c: IndepCstr => if(c.functions.nonEmpty) this.indepConstraints ::= c
      case c: NotIndepCstr => if(c.functions.nonEmpty) this.notIndepConstraints ::= c
      case c: NoneIndepCsrt => if(c.functions.nonEmpty) this.notIndepConstraints ++= c.functions.toSet.subsets(2).map(x => NotIndepCstr(x.toList))
      case c: DalLevelCstr => this.dalConstraints ::= c
      case c: DalqIdentCstr =>
        this.dalConstraints ::= c
      case c@LatentCstr(fm, itvs) => {
        this.latentConstraints ::= c
        fm.setLatent().setCheckIntervals(itvs)
      }
      case c@LambdaBoundsCstr(fm, l, u) => {
        this.boundsConstraints ::= c
        fm.setBounds(l, u)
      }
      case c: AircraftData => this.aircraftData = Some(c)
      case c: UserDefinedOptimisation => optCriteria ::= c
      case c: DalRuleConstraint => dalRule = c
    }
    this
  }

  /** iterates on all contents */
  def iterator = if (aircraftData.isDefined) Iterator(aircraftData.get) else Iterator.empty ++
    allocConstraints.sortBy(_.toString()).iterator ++
    colocConstraints.sortBy(_.toString()).iterator ++
    indepConstraints.sortBy(_.toString()).iterator ++
    dalConstraints.sortBy(_.toString()).iterator ++
    latentConstraints.sortBy(_.toString()).iterator ++
    boundsConstraints.sortBy(_.toString()).iterator ++
    Iterator(dalRule) ++
    optCriteria.sortBy(_.toString()).iterator ++
    costConstraints.sortBy(_.toString()).iterator

  /** Saves the object in textual form to a file. */
  def save(filename: String) = {
    val file = new BufferedWriter(new FileWriter(filename))
    iterator.foreach(e => file.write(e.toString + "\n"))
    file.close
  }
}

