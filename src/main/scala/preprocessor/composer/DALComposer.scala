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

import dalculator.cli.DalculatorParameters
import dalculator.model.DalLevel._
import dalculator.model._
import dalculator.solver.SolveDal
import dalculator.utils.Configuration
import preprocessor.analysis.AnalysisTypes.{DALAnalysis, FHA, MCSAnalysis, UDEFFile}
import preprocessor.ast._
import preprocessor.transformers.Parser

import java.io.FileWriter
import scala.collection.mutable

trait DALComposer[Pre] extends Composer[Pre] {
  type Result = DALAnalysis
}

trait DALComposerInstances {

  implicit def UDEFFileToDALComposer(implicit parser: Parser.Aux[UDEFFile, Map[Item, DalLevel]], conf:Configuration): DALComposer[UDEFFile] = new DALComposer[UDEFFile] {

    def apply(pre: UDEFFile): DALAnalysis = DALAnalysis(parser.parse(pre))
  }

  implicit object MCSWithRuleToDALSATComposer extends DALComposer[(MCSAnalysis, Map[FC, (Int, DalLevel)], UserDefinedConstraints, DalRule)] {
    def apply(pre: (MCSAnalysis, Map[FC, (Int, DalLevel)], UserDefinedConstraints, DalRule)): DALAnalysis = {
    val params = new DalculatorParameters()
    params.dalRule = pre._4
    params.dalUdef = pre._3
    val fm = new Model()
    val modelItems = mutable.Set.empty[Item]
    for {
      (fc, mcs) <- pre._1.result
      (nSev, dalRef) = pre._2(fc)
    } yield {
      fm.add(FailureCondition(fc.name.name,
        nSev,
        -5,
        mcs.toList.map(cs => FailureModeList(cs.toList)),
        dalRef
      ))
      modelItems ++= mcs.flatten.map(_.function)
    }
    if (fm.freeze)
      println("[WARNING] some MCS were discarded because they were shorter than nSev, DAL set to reference DAL.")

    // do some work only if some MCS are of the proper size
    if (!(fm.mcs2DalnSevTuples.isEmpty & fm.mcsTooShort.isEmpty)) {
      SolveDal(fm,
        None,
        params.dalUdef,
        params.dalSolver,
        params.dalOpbFile,
        params.dalRule,
        params.dalResultsFile
      ) match {
        case Some(value) =>
          val nonCompliantCS = for {
            (cs, refDal, _) <- fm.mcsTooShort
            itemUid <- cs
          } yield {
            Item.getFromUid(itemUid) -> refDal
          }

          val dalAllocation = value.collect({
            case DalLevelCstr(item, DalRelOp.Eq, dalLevel) if modelItems.contains(item) => item -> dalLevel
            case DalLevelCstr(item, DalRelOp.Ge, dalLevel) if modelItems.contains(item) => item -> dalLevel
            case DalLevelCstr(item, DalRelOp.Lt, dalLevel) if modelItems.contains(item) => item -> (dalLevel - 1)
          })
          DALAnalysis((nonCompliantCS ++ dalAllocation).groupMapReduce(_._1)(_._2)((l, r) => if (l < r) r else l))
        case None => throw new Exception("No satisfying DAL allocation found.")
      }
    } else {
      println("[WARNING] Some cuts do not comply to the nSev requirement, DAL allocation set to trivial pessimistic allocation")
      DALAnalysis(modelItems.map(_ -> pre._2.values.map(_._2).max).toMap)
    }
  }
  }

  implicit object MCSToDALSATComposer extends DALComposer[(MCSAnalysis, Map[FC, (Int, DalLevel)], UserDefinedConstraints)] {
    def apply(pre: (MCSAnalysis, Map[FC, (Int, DalLevel)], UserDefinedConstraints)): DALAnalysis =
      MCSWithRuleToDALSATComposer((pre._1,pre._2,pre._3, DalRule1))
  }

  implicit object MCSToDALIDPComposer extends DALComposer[(MCSAnalysis, FHA)] {

    def apply(pre: (MCSAnalysis, FHA)): DALAnalysis = ???

    def mkName(s: Iterable[FailureMode]): String = {
      "s" + s.map(e => mkName(e)).mkString("")
    }

    def mkName(e: FailureMode): String = {
      e.name.s.replace(".", "_")
    }

    def mkName(fc: FC): String = {
      fc.name.name.replace(".", "_")
    }

    def exportDALIDP(analyse: MCSAnalysis, fha: FHA, writer: FileWriter): Unit = {
      val cost = (d: DalLevel, e: FailureMode) => d match {
        case DalA => 20
        case DalB => 15
        case DalC => 5
        case DalD => 4
        case DalE => 0
      }
      exportDALIDP(analyse, fha, writer, cost)
    }

    def exportDALIDP(analyse: MCSAnalysis, fha: FHA, writer: FileWriter, cost: (DalLevel, FailureMode) => Int): Unit = {
      val funs: Set[FailureMode] = analyse.result.values.flatMap(mcs => mcs.flatten).toSet
      val funsets: Set[Set[FailureMode]] = analyse.result.values.flatten.flatMap(c => c.subsets().map(_.toSet).filterNot(_.isEmpty)).toSet
      writer.write(
        s"""vocabulary V {
           |     type Cost isa int
           |    type MCS
           |    type FunSet
           |    type Fun constructed from ${funs.map(s => mkName(s)).toSeq.sorted.mkString("{", ",\n", "}")}
           |    type Card isa int
           |    type DAL isa int
           |    type NSEV isa int
           |    type Option constructed from {None,Option1,Option2}
           |    funSetInMCS (FunSet, MCS)
           |    funInFunSet (Fun, FunSet)
           |    included(FunSet,FunSet)
           |    indep(Fun,Fun)
           |    indepm(FunSet)
           |    kard(FunSet) : Card
           |    ndal(Fun):DAL
           |    NDAL(MCS):DAL
           |    nsev(DAL):NSEV
           |    indepCore(Fun,Fun)
           |    sizeGE(FunSet,Card)
           |    oracle(FunSet):Option
           |    cost(DAL,Fun):Cost
           |    totalCost:Cost
           |    maxDAL:DAL
           |    }
           |
       """.stripMargin)

      writer.write(
        """
          |theory T : V {
          |    //definition of indepm
          |    {
          |        ! m[FunSet]: indepm(m) <-
          |    		(!p[Fun] q[Fun] :
          |    			(funInFunSet(p, m) & funInFunSet(q, m) & p ~= q) => indep(p,q)).
          |    }
          |
          |    //definition of size
          |    {
          |
          |        ! mc[FunSet] n[int]: sizeGE(mc, n) <-
          |    		(?m[FunSet]:
          |    			included(m,mc) & kard(m) = n & indepm(m)).
          |    }
          |
          |    //after independence collapse Nsev must be ok
          |    !mcs[MCS] mc[FunSet] :
          |    	funSetInMCS(mc, mcs) & kard(mc) >= nsev(NDAL(mcs)) => sizeGE(mc,nsev(NDAL(mcs))).
          |
          |    !mcs[MCS] mc[FunSet] :
          |    	funSetInMCS(mc, mcs) & kard(mc) < nsev(NDAL(mcs)) => (
          |    		sizeGE(mc,kard(mc)) &
          |    		(!p[Fun]: funInFunSet(p,mc) => ndal(p) = NDAL(mcs))).
          |
          |    //if no degradation DAL of function is the DAL of mcs
          |    !mcs[MCS] mc[FunSet] p[Fun]: (funSetInMCS(mc, mcs) & funInFunSet(p, mc)) =>
          |    	oracle(mc) = None => ndal(p) >= NDAL(mcs).
          |
          |    //dal level at least NDAL - 2
          |    !mcs[MCS] mc[FunSet] p[Fun]:
          |    	(funSetInMCS(mc, mcs) & funInFunSet(p, mc)) =>
          |    		(ndal(p) >= NDAL(mcs) - 2 ).
          |
          |   //option 1 if DAL is downgraded then at least one indep is a initial DAL
          |    !mcs[MCS] mc[FunSet] p[Fun]: (funSetInMCS(mc, mcs) & funInFunSet(p, mc)) =>
          |    	oracle(mc) = Option1 =>
          |    		((ndal(p) >= NDAL(mcs) |
          |    			(ndal(p) < NDAL(mcs)) =>
          |    				(?m[FunSet]:
          |    					included(m,mc)
          |    					& indepm(m)
          |    					& kard(m) >= nsev(NDAL(mcs))
          |    					& funInFunSet(p, m)
          |    					& (?q[Fun]: funInFunSet(q, m) & ndal(q) >= NDAL(mcs))))).
          |
          |    //option 2 if DAL is downgraded then at least two indep are at initial DAL - 1
          |    !mcs[MCS] mc[FunSet] p[Fun]: (funSetInMCS(mc, mcs) & funInFunSet(p, mc)) =>
          |    	oracle(mc) = Option2 =>
          |    		((ndal(p) >= NDAL(mcs) |
          |    			(ndal(p) < NDAL(mcs)) =>
          |    				(?m[FunSet]:
          |    					included(m,mc)
          |    					& indepm(m)
          |    					& kard(m) >= nsev(NDAL(mcs))
          |    					& funInFunSet(p, m)
          |    					& (?q[Fun] r[Fun]:
          |    						funInFunSet(q, m)
          |    						& funInFunSet(r, m)
          |    						& q ~= r
          |    						& ndal(q) >= NDAL(mcs) - 1
          |    						& ndal(r) >= NDAL(mcs) - 1)))).
          |
          |    {
          |        !p[Fun] q[Fun]: indep(p,q) <- indepCore(p,q).
          |        !p[Fun] q[Fun]: indep(q,p) <- indepCore(p,q).
          |        !p[Fun]: indep(p,p) <- true.
          |    }
          |
          |    totalCost = sum{f[Fun] : true : cost(ndal(f),f)}.
          |
          |    maxDAL = max{f[Fun]:true: ndal(f)}.
          |}
          |
          |""".stripMargin)

      writer.write(
        """term C:V{
          |    totalCost
          |}
          |
          |term MD:V{
          |    maxDAL
          |}
          |""".stripMargin)


      writer.write(
        s"""
           |structure S:V{
           |    Cost = {0..100}
           |    DAL = {0..4}
           |    NSEV = {1;2;3}
           |    MCS = ${analyse.result.map(kv => mkName(kv._1)).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    Card = {1..${analyse.result.map(kv => kv._2.map(_.size).max).max}}
           |    FunSet = ${funsets.map(s => mkName(s)).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    funSetInMCS = ${analyse.result.flatMap(kv => kv._2.map(c => s"${mkName(c)},${mkName(kv._1)}")).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    funInFunSet = ${funsets.flatMap(c => funs.intersect(c).map(e => s"${mkName(e)},${mkName(c)}")).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    included=${funsets.flatMap(c => c.subsets().filterNot(_.isEmpty).map(s => s"${mkName(s)},${mkName(c)}")).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    kard=${funsets.map(c => s"${mkName(c)} -> ${c.size}").toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    NDAL = ${analyse.result.map(kv => s"${mkName(kv._1)} -> ${fha.result.getOrElse(kv._1, CAT).toInt}").toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |    nsev = {
           |      ${DalA.intValue} -> 3;
           |      ${DalB.intValue} -> 2;
           |      ${DalC.intValue} -> 1;
           |      ${DalD.intValue} -> 1;
           |      ${DalE.intValue} -> 1
           |    }
           |    cost = ${DalLevel.values.flatMap(d => funs.map(f => s"${d.intValue},${mkName(f)} -> ${cost(d, f)}")).toSeq.sorted.mkString("{\n\t\t", ";\n\t\t", "\n\t\t}")}
           |}
           |
       """.stripMargin)

      writer.write(
        """
          |procedure main() {
          |  // stdoptions.nbmodels = 0
          |  // printmodels(modelexpand(T,S))
          |   print(minimize(T,S,C)[1])
          |}
        """.stripMargin)
      writer.flush()
    }
  }
}
