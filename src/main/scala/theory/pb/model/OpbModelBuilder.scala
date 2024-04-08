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
package theory.pb.model

import theory.pb.solver.{OpbTrivialSat, OpbTrivialUnsat}

import scala.collection.immutable.HashSet

/** A class used as interface to build an pseudo-Boolean model. */
class OpbModelBuilder {

  /**
   * Number of fresh variables created in the pseudo-boolean model.
   * */
  var nofVar: Int = 1

  /**
   * List of pseudo-boolean constraints of the model.
   * */
  var opbConstr: Vector[OpbConstraint] = Vector.empty

  /**
   * List of unit clauses of the system. (contains x1 which represents true by convention).
   */
  var clauses1: Vector[Int] = Vector.empty :+ 1

  /**
   * List of 2-clauses of the system.
   */
  var clauses2: Vector[(Int, Int)] = Vector.empty

  /**
   * List of 3-clauses of the system.
   */
  var clauses3: Vector[(Int, Int, Int)] = Vector.empty

  /**
   * List of n-clauses of the system.
   */
  var clausesN: Vector[List[Int]] = Vector.empty

  /**
   * Returns a fresh pseudo-boolean variable.
   * TODO check for integer overflow on variable counter.
   * */
  def genVar: Int = {
    nofVar += 1; nofVar
  }


  /**
   * Adds a unit clause to the pseudo-boolean model.
   * */
  def addClause1(lit: Int): Unit = lit match {
    case -1 =>
      println("found contradiction during OpbModel construction.")
      throw OpbTrivialUnsat
    case 1 =>
    /* discard trivially true clause */
    case _ =>
      clauses1 = clauses1 :+ lit
  }

  /**
   * Adds a 2-clause to the pseudo-boolean model.
   * */
  def addClause2(lit1: Int, lit2: Int): Unit = {
    if (lit1 == 1 || lit2 == 1) {
      /* discard true clause */
    } else if (lit1 == -1) {
      addClause1(lit2)
    } else if (lit2 == -1) {
      addClause1(lit1)
    } else if (lit1 == -lit2) {
      // clause contains both a literal and its negation, it is trivially satisfied
      throw OpbTrivialSat
    } else {
      clauses2 = clauses2 :+ (lit1, lit2)
    }
  }

  /**
   * Adds a 3-clause to the pseudo-boolean model.
   * */
  def addClause3(lit1: Int, lit2: Int, lit3: Int): Unit = {
    if (lit1 == 1 || lit2 == 1 || lit3 == 1) {
      /* discard true clause */
    } else if (lit1 == -1) {
      addClause2(lit2, lit3)
    } else if (lit2 == -1) {
      addClause2(lit1, lit3)
    } else if (lit3 == -1) {
      addClause2(lit1, lit2)
    } else if (lit1 == -lit2 || lit1 == -lit3 || lit2 == -lit3) {
      // clause contains both a literal and its negation, it is trivially satisfied
      throw OpbTrivialSat
    } else {
      clauses3 = clauses3 :+ (lit1, lit2, lit3)
    }
  }

  /**
   * Adds a clause to the pseudo-boolean model.
   * */
  def addClauseN(lits: List[Int]): Unit = {
    // remove false literals, detect contradictions and statically true clauses
    try {
      val filtered = lits.foldLeft(new HashSet[Int])((res, x) => {
        if (x == 1) {
          throw OpbTrivialSat
        } else if (res.contains(-x)) {
          // clause contains both a literal and its negation, it is trivially satisfied
          throw OpbTrivialSat
        } else {
          res + x
        }
      })

      filtered.toList match {
        case Nil =>
          println("found contradiction during OpbModel construction.")
          throw OpbTrivialUnsat
        case lit1 :: Nil => addClause1(lit1)
        case lit1 :: lit2 :: Nil => addClause2(lit1, lit2)
        case lit1 :: lit2 :: lit3 :: Nil => addClause3(lit1, lit2, lit3)
        case l@_ => clausesN = clausesN :+ l
      }

    } catch {
      case OpbTrivialUnsat =>
        println("found contradiction during OpbModel construction.")
        throw OpbTrivialUnsat
      case OpbTrivialSat => /* discard trivially true clause*/
    }
  }

  /**
   * Adds a general pseudo boolean constraint to the pseudo-boolean model.
   * */
  def addConstraint(sum: LitSum, op: OpbRelOp, cte: Int): Unit = {
    try {
      opbConstr = opbConstr :+ OpbConstraint(sum, op, cte)
    } catch {
      case OpbTrivialSat => /* Discard trivially true constraint. */
      case OpbTrivialUnsat =>
        println("Statically false constraint.")
        throw new Exception("Statically false constraint.")
    }
  }

  /**
   * Returns a Model made from the current builder's state with the given criterion.
   * */
  def get(criterion: OpbCriterion = OpbSolve): OpbModel = {
    val header = OpbHeader(nofVar, clauses1.size + clauses2.size + clauses3.size + clausesN.size + opbConstr.size)
    val res = OpbModel(header, List(criterion), clauses1, clauses2, clauses3, clausesN, opbConstr, Vector.empty :+ "Created by OpbBuilder")
    res
  }

  /** Returns a Model made from the current builder's state with the given list of criteria for leximin optimization. */
  def get(criteria: List[OpbCriterion]): OpbModel = {
    val header = OpbHeader(nofVar, clauses1.size + clauses2.size + clauses3.size + clausesN.size + opbConstr.size)
    val res = OpbModel(header, criteria, clauses1, clauses2, clauses3, clausesN, opbConstr, Vector.empty :+ "Created by OpbBuilder")
    res
  }

  /**
   * Returns the true literal.
   * */
  def genTrue = 1

  /**
   * Returns the false literal.
   * */
  def genFalse: Int = -1

  /**
   * Returns the negation of the given literal.
   * */
  def genNot(l: Int): Int = -l

  /**
   * Generates a fresh literal x representing the conjunction of the elements of l.
   * The conjunction is encoded using clauses as follows :
   * {{{
   * And(a1,a2) ~~~> x
   * (~a1, ~a2, x)
   * (a1, ~x)
   * (a2, ~x)
   * }}}
   * */
  def genAnd(l1: Int, l2: Int): Int = {
    val x = genVar
    addClause3(x, -l1, -l2)
    addClause2(l1, -x)
    addClause2(l2, -x)
    genAnd(List(l1, l2))
  }

  /**
   * Generates a fresh literal x representing the conjunction of the elements of l.
   * The conjunction is encoded using clauses as follows :
   * {{{
   * And(a1,...,an) ~~~> x
   * (~a1, ... , ~an, x)
   * (a1, ~x)
   * ...
   * (an, ~x)
   * }}}
   * */
  def genAnd(l: List[Int]): Int = {
    l.length match {
      case 0 => 1
      case 1 => l.head
      case _ =>
        val x = genVar
        if (l.size == 2) {
          addClause3(x, -l.head, -l(1))
        } else {
          addClauseN(x :: l.map(k => -k))
        }
        l.foreach(k => addClause2(k, -x))
        x
    }
  }

  /** Generates a fresh literal x representing the disjunction of the literals.
   *
   * The disjunction is encoded using clauses as follows :
   * {{{
   * Or(a1, a1) ~~~> x
   * (a1, a2, ~x)
   * (~a1, x)
   * (~a2, x)
   * }}}
   * */
  def genOr(l1: Int, l2: Int): Int = {
    val x = genVar
    addClause3(-x, l1, l2)
    addClause2(x, -l1)
    addClause2(x, -l2)
    x
  }

  /** Generates a fresh literal x representing the disjunction of the literals.
   *
   * The disjunction is encoded using clauses as follows :
   * {{{
   * Or(a1,...,an) ~~~> x
   * (a1, ... , an, ~x)
   * (~a1, x)
   * ...
   * (~an, x)
   * }}}
   * */
  def genOr(l: List[Int]): Int = {
    l.length match {
      case 0 => -1
      case 1 => l.head
      case _ =>
        val x = genVar
        if (l.size == 2) {
          addClause3(-x, l(0), l(1))
        } else {
          addClauseN(-x :: l)
        }
        l.foreach(k => addClause2(-k, x))
        x
    }
  }

  /**
   * Generates a fresh literal representing logical implication of given literals.
   * {{{
   * Implies(p, q) ~~~> x
   * (~p, q, ~x)
   * ( p, x)
   * (~q, x)
   * }}}
   * */
  def genImplies(p: Int, q: Int): Int = {
    val x = genVar
    addClause3(-p, q, -x)
    addClause2(p, x)
    addClause2(-q, x)
    x
  }

  /**
   * Generates a fresh literal representing logical implication of given literals.
   * {{{
   * Equiv(p, q) ~~~> x
   * (~p,  q, ~x)
   * ( p, ~q, ~x)
   * (~p, ~q,  x)
   * ( p,  q,  x)
   * }}}
   * */
  def genEquiv(p: Int, q: Int): Int = {
    val x = genVar
    addClause3(-p, q, -x)
    addClause3(p, -q, -x)
    addClause3(-p, -q, x)
    addClause3(p, q, x)
    x
  }

  /**
   * Returns a freshLiteral encoding the if then else of the given literals.
   * {{{
   * Ite(s, t, f) ~~~> x
   * (~s, ~t,  x)
   * (~s,  t, ~x)
   * ( s, ~f,  x)
   * ( s,  f, ~x)
   * (~t, ~f,  x)
   * ( f,  t, ~x)
   * }}}
   * */
  def genIte(s: Int, t: Int, f: Int): Int = {
    val x = genVar
    addClause3(-s, -t, x)
    addClause3(-s, t, -x)
    addClause3(s, -f, x)
    addClause3(s, f, -x)
    addClause3(-t, -f, x)
    addClause3(t, f, -x)
    x
  }

  /**
   * Returns a LitSum representing the given integer constant.
   */
  def genIntConst(i: Int): LitSum = LitSum(Vector.empty, i)

  /**
   * Returns a LitSum representing Ite(c, s1, s2).
   * */
  def genIte(cond: Int, s1: LitSum, s2: LitSum): LitSum = {
    val l1WithCond = (if (s1.cte != 0) Vector.empty :+ (cond, s1.cte) else Vector.empty) ++ s1.litsAndCoeffs.map(x => (genAnd(x._1, cond), x._2))
    val l2WithCond = (if (s2.cte != 0) Vector.empty :+ (-cond, s2.cte) else Vector.empty) ++ s2.litsAndCoeffs.map(x => (genAnd(x._1, -cond), x._2))
    LitSum(l1WithCond ++ l2WithCond, 0)
  }

  /**
   * Returns a LitSum in which all literals of the original sum are and-ed with the condition.
   * Sigma(ai * And(c, li)).
   * */
  def genAnd(cond: Int, s: LitSum): LitSum = {
    val newS = s.litsAndCoeffs.map(x => (genAnd(x._1, cond), x._2))
    if (s.cte != 0)
      LitSum(newS :+ (cond, s.cte), 0)
    else
      LitSum(newS, 0)
  }

  /**
   * Returns a LitSum representing the sum of s1 and s2.
   * */
  def genSum(s1: LitSum, s2: LitSum): LitSum = LitSum(s1.litsAndCoeffs ++ s2.litsAndCoeffs, s1.cte + s2.cte)

  /** Returns a LitSum representing the population count of the given literals. */
  def genPopCount(lits: List[Int]): LitSum = LitSum(Vector.empty ++ lits.map(l => (l, 1)), 0)

  /** Returns a fresh literal encoding {{{ s1 Eq s2 }}}. */
  def genEq(s1: LitSum, s2: LitSum): Int = genAnd(genGe(s1, s2), genLe(s1, s2))

  /** Returns a fresh literal encoding  {{{ s1 Ge s2 }}}.
   *
   * The following constraints are used to encode the semantics of the variable:
   * {{{
   *                a1.l1 + .. + an.ln >= i ~~~> x
   *         i.~x + a1.l1 + .. + an.ln >= i
   * Sigma(ai).~x + a1.l1 + .. + an.ln <= Sigma(ai)+i-1
   * }}}
   * */
  def genGe(s1: LitSum, s2: LitSum): Int = genLe(s2, s1)

  /**
   * Returns a fresh literal encoding  {{{ s1 Gt s2 }}}.
   */
  def genGt(s1: LitSum, s2: LitSum): Int = genLe(LitSum(s2.litsAndCoeffs, s2.cte + 1), s1)

  /**
   * Returns a fresh literal encoding  {{{ s1 Lt s2 }}}.
   * */
  def genLt(s1: LitSum, s2: LitSum): Int = genLe(LitSum(s1.litsAndCoeffs, s1.cte + 1), s2)


  /** Returns a fresh literal encoding  {{{ s1 Le s2 }}}
   *
   * The following constraints are used to encode the semantics of the variable:
   * {{{
   *             + a1.l1 + .. + an.ln <= i ~~~> x
   *     (i+1).x + a1.l1 + .. + an.ln >= i+1          (eq. 1)
   * Sigma(ai).x + a1.l1 + .. + an.ln <= Sigma(ai)+i  (eq. 2)
   * }}}
   *
   * when x is true, these constraints rewrite to :
   * a1.l1 + .. + an.ln >= 0  (eq. 1) (trivially true)
   * a1.l1 + .. + an.ln <= i  (eq. 2) (encodes the desired expression)
   * (i must be >= 0 otherwise there is a contradiction)
   *
   * when x is false, these constraints rewrite to :
   * a1.l1 + .. + an.ln >= i+1  (eq. 1) (encodes the desired expression)
   * (i+1 must be > 0 otherwise the constraint is trivially true)
   * a1.l1 + .. + an.ln <= Sigma(ai) + i  (eq. 2) (trivially true since even with all literals true, the left part of the inequation cannot exceed Sigma(ai))
   * (Sigma(ai) + i must be >= 0 otherwise there is a contradiction.)
   * Sigma(ai) is necessarily positive since all ai:s are always positive by construction.
   * so we just need i>=0
   *
   * */
  def genLe(s1: LitSum, s2: LitSum): Int = {
    /*
     * a0 + a1.l1 + ...+  an.ln <= a0' + a1'.l1' + ... + an'.ln'
     * a0 + a1.l1 + ...+  an.ln - a0' - a1'.l1' - ... - an'.ln' <= 0
     * a0 + a1.l1 + ...+  an.ln - a0' (-a1' + a1') - a1'.l1' - ... (-an' + an') - an'.ln' <= 0
     * a0 + a1.l1 + ...+  an.ln - a0' -a1' + (a1' - a1'.l1') - ... -an' + (an' - an'.ln') <= 0
     * a0 + a1.l1 + ...+  an.ln - a0' -a1' + a1'.-l1' - ... -an' + an'.-ln' <= 0
     * a1.l1 + ...+ an.ln + a1'.-l1' + ... + an'.-ln' <= a0' + a1' + ... + an' - a0
     *
     */
    val i = s2.litsAndCoeffs.foldLeft(s2.cte)((res, x) => res + x._2) - s1.cte
    val litsAndCoeffs = s1.litsAndCoeffs ++ s2.litsAndCoeffs.map(x => (-x._1, x._2))
    val sigma = litsAndCoeffs.foldLeft(0)((res, x) => res + x._2)
    val x = genVar
    require(i >= 0)
    val n1 = LitSum(litsAndCoeffs :+ (x, i + 1), 0)
    val n2 = LitSum(litsAndCoeffs :+ (x, sigma), 0)
    addConstraint(n1, OpbGe, i + 1)
    addConstraint(n2, OpbLe, i + sigma)
    x
  }


  /**
   * Generates a LitSum representing a integer in base 1.
   * The value of the integer is equal to the number of true bits in the sum.
   * */
  def genIntBase1(nofBits: Int, cte: Int = 0): LitSum = {
    val variables = (0 until nofBits).map(_ => (this.genVar, 1))
    variables.foldLeft(genTrue)((res, x) => {
      addClause1(genImplies(x._1, res)); x._1
    })
    LitSum(Vector.empty ++ variables, cte)
  }

  /**
   * Generates a LitSum representing an integer in base 2.
   * The sum is built as like this :
   * {{{
   * 		2^(0).v_{0} + ... + 2^(nofBits-1).v_{nofBits-1}
   * }}}
   */
  def genIntBase2(nofBits: Int): LitSum = {
    require(nofBits <= 10)
    LitSum(Vector.empty ++ (0 until nofBits).map(x => (this.genVar, BigInt(2).pow(x).intValue)), 0)
  }
}
