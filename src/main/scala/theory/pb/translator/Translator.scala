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
package theory.pb.translator

import theory.Logic.{Sat, _}
import theory.pb.Globals._
import theory.pb.model._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * A translator that builds a pseudo-boolean model from a LogicModel. 
 * */
class Translator(val m: LModel) {

  /** Regroup nested Ands: in single n-ary And. */
  val regroupAnd = true

  /** Regroup nested Or:s in single n-ary Or. */
  val regroupOr = true

  /** Builder interface used to generate the pseudo-boolean model. */
  val builder = new OpbModelBuilder()

  /** Cache of translation results for boolean expressions. */
  var boolCache: HashMap[BoolExp, Int] = HashMap.empty

  /** Cache of translation results for boolean function calls. */
  var boolIdentCache: HashMap[Ident, Int] = HashMap.empty

  /** Stack of translation results for boolean expressions. */
  val boolRes: mutable.Stack[Int] = mutable.Stack.empty

  /** Pushes a result on the stack of boolean expressions. */
  def boolResPush(e: BoolExp, i: Int): Unit = {
    boolCache += (e -> i)
    boolRes.push(i)
  }

  /** Pushes a result on the stack of boolean expressions. */
  def boolResIdentPush(e: Ident, i: Int): Unit = {
    boolIdentCache += (e -> i)
    boolRes.push(i)
  }

  /** Stack of translation results for integer expressions. */
  val intRes: mutable.Stack[LitSum] = mutable.Stack.empty

  /** Translation cache for integer expressions. */
  var intCache: HashMap[IntExp, LitSum] = HashMap.empty


  /** Pushes a result on the stack of boolean expressions. */
  def intResPush(e: IntExp, s: LitSum): Unit = {
    intCache += (e -> s)
    intRes.push(s)
  }


  def dumpCache(filename: String): Unit = {
    print("dumping translation cache... ")
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(filename))
    boolCache.foreach(item => {
      val (e, lit) = item
      if (lit > 0) {
        bw.write("x" + lit.toString + " -> " + e.toString + "\n")
      }
    })
    boolIdentCache.foreach(item => {
      val (e, lit) = item
      if (lit > 0) {
        bw.write("x" + lit.toString + " -> " + e.toString + "\n")
      }
    })
    intCache.foreach(item => {
      val (e, lit) = item
      bw.write(lit.toString + " -> " + e.toString + "\n")
    })
    bw.close()
  }

  /** Stack of continuations which drives the translation. */
  val todo: mutable.Stack[() => Unit] = mutable.Stack.empty

  /** Translates a Logical Model to a Model. */
  def get: (OpbModel, BackTranslatorFactory) = {
    // translate all constraints
    print("Translating constraints to OPB ...")
    var startTime = System.currentTimeMillis
    m.constraints.foreach(c => {
      translateAsConstraint(c)
    })
    println("done (" + (startTime - System.currentTimeMillis) + "ms).")
    // create the pseudo-boolean model and return it
    print("Translating criteria to OPB ...")
    startTime = System.currentTimeMillis
    val criteria = m.criteria map {
      translate
    }
    println("done (" + (startTime - System.currentTimeMillis) + "ms).")
    (builder.get(criteria), new BackTranslatorFactory(boolCache, boolIdentCache, intCache))
  }

  /**
   * Translates a logical criterion to a pseudo-boolean criterion.
   *
   * Accepted criteria are of the form
   *
   * (min|max)(if c1 then i1 else 0 + ... + if cn then in else 0)
   *
   * */
  def translate(criterion: LSearch): OpbCriterion = {
    criterion match {
      case LSolve | Sat => OpbSolve
      case LMinimize(e) => OpbMin(translate(e))
      case LMaximize(e) => OpbMax(translate(e))
    }
  }


  /** Trampoline translation for boolean expressions. */
  def translate(e: BoolExp): Int = {
    todo.push(() => translateTramp(e))
    while (todo.nonEmpty) {
      val chunk = todo.pop()
      chunk()
    }
    boolRes.pop()
  }

  /** Trampoline translation for integer expressions. */
  def translate(e: IntExp): LitSum = {
    todo.push(() => translateTramp(e))
    while (todo.nonEmpty) {
      val chunk = todo.pop()
      chunk()
    }
    intRes.pop()
  }

  /** Processes the current node and pushes the rest of the computation on the trampoline stack. */
  def translateTramp(e: BoolExp): Unit = e match {
    case c: Ident => boolIdentCache.get(c) match {
      case Some(r) => boolRes.push(r)
      case None => translateTramp1(e)
    }
    case _ => boolCache.get(e) match {
      case Some(r) => boolRes.push(r)
      case None => translateTramp1(e)
    }
  }

  /** Processes the current node and pushes the rest of the computation on the trampoline stack. */
  def translateTramp(e: IntExp): Unit = e match {
    case _ => intCache.get(e) match {
      case Some(r) => intRes.push(r)
      case None => translateTramp1(e)
    }
  }

  def translateTramp1(e: IntExp): Unit = {
    e match {
      case PopCount(x) =>
        todo.push(() => {
          val kids = x.map(_ => boolRes.pop())
          intResPush(e, builder.genPopCount(kids))
        })
        x.foreach(e => todo.push(() => translateTramp(e)))

      case Add(x, y) =>
        todo.push(() => intResPush(e, builder.genSum(intRes.pop(), intRes.pop())))
        todo.push(() => translateTramp(x))
        todo.push(() => translateTramp(y))


      case IntValue(i) =>
        intResPush(e, builder.genIntConst(i))

      case IfInt(c, x, y) =>
        todo.push(() => intResPush(e, builder.genIte(boolRes.pop(), intRes.pop(), intRes.pop())))
        todo.push(() => translateTramp(c))
        todo.push(() => translateTramp(x))
        todo.push(() => translateTramp(y))

      case Mult(x, y) =>
        (x, y) match {
          case (IntValue(i), x@_) =>
            todo.push(() => todo.push(() => intResPush(e, intRes.pop().mult(i))))
            todo.push(() => translateTramp(x))

          case (x@_, IntValue(i)) =>
            todo.push(() => intResPush(e, intRes.pop().mult(i)))
            todo.push(() => translateTramp(x))

          case (_, _) =>
            println("cannot handle non linear operations :" + e.toString)
            throw new Exception("cannot handle non linear operations :" + e.toString)
        }
    }
  }

  def translateTramp1(e: BoolExp): Unit = {

    e match {

      // ------- Components ---------
      case id@Ident(_) => boolResIdentPush(id, builder.genVar)

      // ------- Propositional connectors ---------
      case True => boolResPush(e, builder.genTrue)

      case False => boolResPush(e, builder.genFalse)

      case Not(p) =>
        todo.push(() => boolResPush(e, builder.genNot(boolRes.pop())))
        todo.push(() => translateTramp(p))

      case And(p, q) =>
        if (regroupAnd) {
          val kids = this.inlineAndRec(List(p, q))
          todo.push(() => boolResPush(e, builder.genAnd(kids.map(_ => boolRes.pop()))))
          kids.foreach(x => todo.push(() => translateTramp(x)))
        } else {
          todo.push(() => boolResPush(e, builder.genAnd(boolRes.pop(), boolRes.pop())))
          todo.push(() => translateTramp(p))
          todo.push(() => translateTramp(q))
        }

      case Or(p, q) =>
        if (regroupOr) {
          val kids = this.inlineOrRec(List(p, q))
          todo.push(() => boolResPush(e, builder.genOr(kids.map(_ => boolRes.pop()))))
          kids.foreach(x => todo.push(() => translateTramp(x)))
        } else {
          todo.push(() => boolResPush(e, builder.genOr(boolRes.pop(), boolRes.pop())))
          todo.push(() => translateTramp(p))
          todo.push(() => translateTramp(q))
        }

      case IfBool(k1, k2, k3) =>
        todo.push(() => boolResPush(e, builder.genIte(boolRes.pop(), boolRes.pop(), boolRes.pop())))
        todo.push(() => translateTramp(k1))
        todo.push(() => translateTramp(k2))
        todo.push(() => translateTramp(k3))

      // ------- Relational Operators ---------
      case LeInt(x, y) =>
        todo.push(() => boolResPush(e, builder.genLe(intRes.pop(), intRes.pop())))
        todo.push(() => translateTramp(x))
        todo.push(() => translateTramp(y))

      // ------- Relational Operators ---------
      case EqInt(x, y) =>
        todo.push(() => boolResPush(e, builder.genEq(intRes.pop(), intRes.pop())))
        todo.push(() => translateTramp(x))
        todo.push(() => translateTramp(y))

      case _ =>
        println("The following BoolExp cannot be translated to OPB: " + e.getClass)
        throw new Exception("The following BoolExp cannot be translated to OPB: " + e.getClass)
    }
  }

  /** Recursively in-lines nested disjunctions to return a list of expressions representing an n-ary disjunction. */
  @tailrec
  private def inlineOrRec(current: List[BoolExp], result: List[BoolExp] = Nil): List[BoolExp] = {
    current match {
      case Nil => result
      case Or(p, q) :: t => inlineOrRec(t reverse_::: List(p, q), result)
      case _ => inlineOrRec(current.tail, current.head :: result)
    }
  }

  /** Recursively in-lines nested conjunctions to return a list of expressions representing an n-ary conjunction. */
  @tailrec
  private def inlineAndRec(current: List[BoolExp], result: List[BoolExp] = Nil): List[BoolExp] = {
    current match {
      case Nil => result
      case And(p, q) :: t => inlineAndRec(t reverse_::: List(p, q), result)
      case _ => inlineAndRec(current.tail, current.head :: result)
    }
  }


  /** Translates a Boolean expression directly as a constraint in the PBModel,
   * without introducing a fresh variable to represent the expression.
   * */
  def translateAsConstraint(e: BoolExp): Unit = {

    e match {

      // Top level And:s are expanded to individual constraints
      case And(p, q) =>
        translateAsConstraint(p)
        translateAsConstraint(q)

      // Nested Or:s are all recursively in-lined and transformed in a single clause
      case Or(p, q) =>
        builder.addClauseN(inlineOrRec(List(p, q)).map(translate))

      case _ =>
        builder.addClause1(translate(e))
    }
  }

  /** Translates n-ary relational expressions of the form
   * e1 <op> ... <op> en to a list of literals  (e1 <op> e2)  (e2 <op> e3) ... (en-1 <op> en)
   *
   * @param l         The List(e1, ..., en)
   * @param genConstr The builder function generating a literal xi such that  xi <=> ei <op> ei+1.
   *
   *                  genConstr gets called on all pairs of LitSum resulting from the translation of ei,
   *                  the list of resulting xi is returned.
   *                  TODO :  cache reuse is not done in this function
   */
  def trNaryRelOp(l: List[IntExp], genConstr: (LitSum, LitSum) => Int): List[Int] = {

    /** for a pair of functions */
    def trRelOp2(genConstr: (LitSum, LitSum) => Int)(prev: (Option[LitSum], List[Int]), e2: IntExp): (Option[LitSum], List[Int]) = {
      val (opts1, res) = prev
      opts1 match {
        case None => (Some(translate(e2)), res)
        case Some(s1) =>
          // translate both sides, put variables to the left, constants to the right
          val s2 = translate(e2)
          val x = genConstr(s1, s2)
          (Some(s2), x :: res)
      }
    }

    // 'res' now contains the literals introduced for binary constraints
    val (_, res) = l.foldLeft[(Option[LitSum], List[Int])]((None, Nil))(trRelOp2(genConstr))
    res
  }
}
