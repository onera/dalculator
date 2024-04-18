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

package dalculator.translator

import dalculator.cli.DalculatorParameters
import dalculator.model._
import theory.pb.solver.{OpbSolver, Sat4jBoth, Sat4jCP, Sat4jRes, WBOCores, WBOLinear, WBOLinearCores}

import java.io.{FileNotFoundException, FileReader}
import scala.util.parsing.combinator.RegexParsers

/** A parser for failure condition files and user defined constraints files and dalculator parameter files. */
object ModelParser extends RegexParsers {

  // ------------------------------ FAILURE CONDITIONS --------------------------------

  /** Rule for file paths (not sanity checked). */
  def filePath: Parser[String] = """[0-9a-zA-Z_./~-]*""".r

  /** Rule for integer numbers. */
  def integer: Parser[Int] = """[0-9][0-9]*""".r ^^ (s => s.toInt)

  /** Rule for decimal numbers. */
  def decimal: Parser[BigDecimal] = """(-)?([0-9]+).[0-9]*""".r ^^ (s => BigDecimal(s))

  /** Rule for function identifiers. */
  def ident: Parser[String] = """[a-zA-Z_][0-9a-zA-Z_.]*""".r

  /** Rule for single quoted identifiers (parser removes quotes). */
  def qident: Parser[String] = "'" ~> ident <~ "'"

  /** Rule for a minmal cut set (a list of quoted identifiers). */
  def cut: Parser[FailureModeList] = ("{" ~> repsep(qident, ",") <~ "}") ^^ (l => FailureModeList(l.map(f => FailureMode(f))))

  /** Rule for lists of minimal cut sets */
  def cuts: Parser[List[FailureModeList]] = rep(cut)

  /** Rule for minimal cut sets header, contains name. */
  def fcname: Parser[String] = "products(MCS(" ~> qident <~ "))" ~ "="

  /** C++ like comments */
  def comment: Parser[String] = "(/\\*([^*]|[\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+/)|(//.*)".r

  /** Top level rule for failure condition files.
   *
   * @param model  the parsed FC is added to this model
   * @param nSev   the given nSev value is assigned to the parsed FC
   * @param xSev   the given xSev value is assigned to the parsed FC
   * @param refDal the given refDal is assigned to the parsed FC
   * */
  def fc(model: Model, nSev: Int, xSev: BigDecimal, refDal: DalLevel): Parser[Model] = opt(comment) ~> fcname ~ cuts <~ "end" ~ opt(comment) ^^ {
    case name ~ cuts =>
      model.add(FailureCondition(name, nSev, xSev, cuts, refDal))
      model
  }

  /** Loads a seq file from a file, writing it in the given model.
   *
   * @param model  the parsed FC is added to this model
   * @param nSev   the given nSev value is assigned to the parsed FC
   * @param xSev   the given xSev value is assigned to the parsed FC
   * @param refDal the given refDal is assigned to the parsed FC
   */
  def loadFC(filename: String, model: Model, nSev: Int, xSev: BigDecimal, refDal: DalLevel): Unit = {
    val reader = try {
      new FileReader(filename)
    } catch {
      case _: FileNotFoundException =>
        println("Error : Could not find file " + filename)
        sys.exit(1)
    }

    parseAll(fc(model, nSev, xSev, refDal), reader) match {
      case _: Success[_] =>
      case Error(msg, in) => println("error " + msg + "\n" + in.pos.longString)
      case Failure(msg, in) => println("failure " + msg + "\n" + in.pos.longString)
    }
    reader.close()
  }

  // ------------------------------ DALCULATOR COMMAND SCRIPT PARAMS --------------------------------

  /** Dal level parser. */
  def dalLevel: Parser[DalLevel] =
    """[ABCDE]""".r ^^ (l => DalLevel(l))

  /** Command for loading a seq file */
  def loadSeqCmd(params: DalculatorParameters): Parser[Unit] =
    ("loadSeq(" ~> (dalLevel <~ ",") ~ (integer <~ ",") ~ (decimal <~ ",") ~ filePath <~ ")") ^^ {
      case refDal ~ nSev ~ xSev ~ path =>
        println("loadSeq", refDal, nSev, xSev, path)
        params.seqFiles ::= path
        params.nSevs ::= nSev
        params.xSevs ::= xSev
        params.refDals ::= refDal
    }

  def integerParam: Parser[String] = "resourceMaxNbr" | "resourceMinNbr"

  def setIntegerParamCmd(params: DalculatorParameters): Parser[Unit] =
    ((integerParam <~ "=") ~ integer) ^^ {
      case param ~ value =>
        println(param, value)
        param match {
          case "resourceMinNbr" => params.indepResourcesStart = value
          case "resourceMaxNbr" => params.indepResourcesStop = value
        }
    }

  def booleanParam: Parser[String] = "indepEnabled" | "indepSaveAlloc" | "dalEnabled" | "dalImportIndep" | "budgetEnabled" | "budgetImportIndep"

  def booleanValue: Parser[Boolean] = ("true" | "false") ^^ {
    case "true" => true
    case "false" => false
  }

  def setBooleanParamCmd(params: DalculatorParameters): Parser[Unit] = ((booleanParam <~ "=") ~ booleanValue) ^^ {
    case param ~ value =>
      println(param, value)
      param match {
        case "indepEnabled" => params.indepEnabled = value
        case "dalEnabled" => params.dalEnabled = value
        case "dalImportIndep" => params.dalImportIndep = value
        case "budgetEnabled" => params.budgetEnabled = value
        case "budgetImportIndep" => params.budgetImportIndep = value
        case "indepSaveAlloc" =>  params.indepSaveAlloc = value
        case _ =>
      }
  }

  def solverValue: Parser[OpbSolver] = ("sat4jboth" | "sat4jcp" | "sat4jres" | "wbocores" | "wbolinear" | "wbolinearcores") ^^ {
    case "sat4jboth" => Sat4jBoth
    case "sat4jcp" => Sat4jCP
    case "sat4jres" => Sat4jRes
    case "wbocores" => WBOCores
    case "wbolinear" => WBOLinear
    case "wbolinearcores" => WBOLinearCores
  }

  def solverParam: Parser[String] = "indepSolver" | "solver" | "dalSolver"

  def setSolverParamCmd(params: DalculatorParameters): Parser[Unit] = ((solverParam <~ "=") ~ solverValue) ^^ {
    case param ~ value =>
      println(param, value)
      param match {
        case "indepSolver" | "solver" => params.indepSolver = value
        case "dalSolver" => params.dalSolver = value
      }
  }

  def pathParam: Parser[String] = "resultsFile" | "opbFile" | "indepResultsFile" | "dalResultsFile" | "indepOpbFile" | "dalOpbFile" | "indepUdefFile" | "dalUdefFile" | "udefFile"

  def setPathParamCmd(params: DalculatorParameters): Parser[Unit] = ((pathParam <~ "=") ~ filePath) ^^ {
    case param ~ value =>
      println(param, value)
      param match {
        case "resultsFile" | "indepResultsFile" => params.indepResultsFile = value
        case "opbFile" | "indepOpbFile" => params.indepOpbFile = value
        case "indepUdefFile" | "udefFile" =>
          params.indepUdefFile = Some(value)
        case "dalResultsFile" => params.dalResultsFile = value
        case "dalOpbFile" => params.dalOpbFile = value
      }
  }

  def paramCmd(params: DalculatorParameters): Parser[Unit] = {
    setPathParamCmd(params) | setBooleanParamCmd(params) | setIntegerParamCmd(params) | setSolverParamCmd(params) | loadSeqCmd(params)
  }

  def cmdFileParser(params: DalculatorParameters): Parser[_] = rep(paramCmd(params))

  /** Loads the given user defined constraints file. */
  def loadParamsFile(filename: String, params: DalculatorParameters): Unit = {
    val reader = try {
      val reader = new FileReader(filename)
      reader
    } catch {
      case _: FileNotFoundException =>
        println("Error : could not find file '%s'".format(filename))
        sys.exit(1)
    }
    parseAll(cmdFileParser(params), reader) match {
      case _: Success[_] => println("successfully parsed file.")
      case Error(msg, in) => println("error " + msg + "\n" + in.pos.longString)
      case Failure(msg, in) => println("failure " + msg + "\n" + in.pos.longString)
    }

    reader.close()
  }

  // ------------------------------ USER DEFINED CONSTRAINTS --------------------------------

  /** Rule for user defined collocation constraints */
  def alloc: Parser[UserDefinedConstraint] = ("Alloc(" ~> repsep(qident, ",") ~ "," ~ integer <~ ")") ^^ {
    case l ~ _ ~ i => AllocCstr(l.map(fname => Item(fname)), i)
  }

  /** Rule for user defined collocation constraints.
   * Unknown identifiers will be discarded
   * FIXME Do not override identifier check => need to parse MCS first
   */
  def coloc: Parser[UserDefinedConstraint] = ("Coloc(" ~> repsep(qident, ",") <~ ")") ^^ (l =>
    ColocCstr(l.map(fname => Item(fname))))

  /** Rule for user defined independence constraints.
   * Unknown identifiers will be discarded
   */
  def indep: Parser[UserDefinedConstraint] = ("Indep(" ~> repsep(qident, ",") <~ ")") ^^ (l => IndepCstr(l.flatMap(fname => Item.get(fname))))

  /** Rule for user defined independence constraints.
   * Unknown identifiers will be discarded
   */
  def notindep: Parser[UserDefinedConstraint] = ("NotIndep(" ~> repsep(qident, ",") <~ ")") ^^ (l => NotIndepCstr(l.flatMap(fname => Item.get(fname))))

  /** Rule for user defined independence constraints.
   * Unknown identifiers will be discarded
   */
  def noneIndep: Parser[UserDefinedConstraint] = ("NoneIndep(" ~> repsep(qident, ",") <~ ")") ^^ (l => NoneIndepCsrt(l.flatMap(fname => Item.get(fname))))

  /** Rule for user defined bounds on DAL levels of functions. */
  def dalBound: Parser[UserDefinedConstraint] = ("Dal(" ~> qident ~ ("Ge" | "Eq" | "Lt") ~ dalLevel <~ ")") ^^ {
    case f ~ op ~ l =>
      val relop = op match {
        case "Ge" => DalRelOp.Ge
        case "Eq" => DalRelOp.Eq
        case "Lt" => DalRelOp.Lt
      }
      DalLevelCstr(Item(f), relop, l)
  }


  /** ***************************************************** */
  /** ***************************************************** */

  /** Rule for user defined relation between DAL levels of two functions. Same as dalBound except is not dalLevel but a function in the second variable */
  def dalBoundqIdent: Parser[UserDefinedConstraint] = ("Dal(" ~> qident ~ ("Ge" | "Eq" | "Lt") ~ qident <~ ")") ^^ {
    case f ~ op ~ g =>
      val relop = op match {
        case "Ge" => DalRelOp.Ge
        case "Eq" => DalRelOp.Eq
        case "Lt" => DalRelOp.Lt
      }
      DalqIdentCstr(Item(f), relop, Item(g))
  }

  /** Rule for user defined a maximal cost that must not be exceeded */
  def maxCost: Parser[UserDefinedConstraint] = ("Cmax =" ~> integer) ^^ MaxCost

  /** Rule for user defined a minimal cost that means each cost has to be greater than minCost */
  def minCost: Parser[UserDefinedConstraint] = ("Cmin =" ~> integer) ^^ MinCost

  /** Rule for the cost defined by the user for a selected DAL */
  def DalCost: Parser[UserDefinedConstraint] = ((("Cost(" ~> qident <~ ", ") ~ dalLevel <~ ") = ") ~ integer) ^^ {
    case c ~ dl ~ i =>
      Cost(Item(c), dl, i)
  }

  /** ***************************************************** */
  /** ***************************************************** */


  /** Dal allocation rules */
  def dalRule: Parser[DalRuleConstraint] = ("DalRule(" ~> ("1" | "2" | "combined") <~ ")") ^^ {
    s => {
      val rule = s match {
        case "1" => DalRule1
        case "2" => DalRule2
        case "combined" => DalRuleCombined
      }
      DalRuleConstraint(rule)
    }
  }


  def dalCard: Parser[UserDefinedCriterion] = ("DalCard" ~> dalLevel) ^^ (l => DalCard(l))

  def indepCard: Parser[UserDefinedCriterion] = "IndepCard" ^^ (l => IndepCard)

  def resourceCard: Parser[UserDefinedCriterion] = "ResourceCard" ^^ (l => ResourceCard)

  /** This is the parser for the criterion cost in the case of the cost optimisation */
  def costCriterion: Parser[UserDefinedCriterion] = "Cost" ^^ (l => CostCriterion)

  def criterion: Parser[UserDefinedCriterion] = dalCard | indepCard | resourceCard | costCriterion

  def optimisation: Parser[UserDefinedOptimisation] = ("OptMin" | "OptMax") ~ ("(" ~> criterion <~ ")") ^^ {
    case opt ~ crit =>
      opt match {
        case "OptMin" => OptMin(crit)
        case "OptMax" => OptMax(crit)
      }
  }

  /** Rule for user defined aircraft data */
  def aircraftData: Parser[UserDefinedConstraint] = ("AircraftProfile(" ~> integer ~ "," ~ "(" ~ repsep(integer, ",") <~ ")" ~ ")") ^^ {
    case t0 ~ _ ~ _ ~ itvs =>
      AircraftData(t0, itvs)
  }

  /** Rule for user defined lambda bounds declaration */
  def lambdaBounds: Parser[UserDefinedConstraint] = ("LambdaBounds(" ~> qident ~ "," ~ decimal ~ "," ~ decimal <~ ")") ^^ {
    case fm ~ _ ~ l ~ _ ~ u =>
      LambdaBoundsCstr(FailureMode(fm), l, u)
  }

  /** Rule for user defined latent failure mode declarations */
  def latent: Parser[UserDefinedConstraint] = ("Latent(" ~> qident <~ ")") ^^ (fm => LatentCstr(FailureMode(fm)))

  /** Rule for user defined latent failure mode declarations with special interval checks */
  def latentItv: Parser[UserDefinedConstraint] = ("Latent(" ~> qident ~ "," ~ repsep(integer, ",") <~ ")") ^^ {
    case fm ~ _ ~ itvs => LatentCstr(FailureMode(fm), Some(itvs))
  }

  /** Disjunction of user defined constraints */
  def usrDefCtr: Parser[UserDefinedConstraint] = alloc | coloc | indep | notindep | noneIndep | dalBound | aircraftData | lambdaBounds | latent | latentItv | optimisation | dalRule | maxCost | minCost | DalCost | dalBoundqIdent

  /** Rule for user defined constraints file */
  def usrDefCtrs(c: UserDefinedConstraints): Parser[UserDefinedConstraints] = opt(rep(usrDefCtr | comment)) ^^ {
    constraints => {
      if (constraints.isDefined)
        constraints.get collect {case cst:UserDefinedConstraint => cst} foreach c.add
    }
      c
  }

  /** Loads the given user defined constraints file. */
  def loadUDef(filename: String, c: UserDefinedConstraints): Unit = {
    val reader = try {
      val reader = new FileReader(filename)
      reader
    } catch {
      case _: FileNotFoundException =>
        println("Error : could not find file '%s'".format(filename))
        sys.exit(1)
    }
    parseAll(usrDefCtrs(c), reader) match {
      case _: Success[_] =>
      case Error(msg, in) => println("error " + msg + "\n" + in.pos.longString)
      case Failure(msg, in) => println("failure " + msg + "\n" + in.pos.longString)
    }

    reader.close()
  }
}
