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

package dalculator.cli

import dalculator.model.{BudgetCriterion, Combined2Criterion, ShortRange}

/** A class storing default parameter values */
object DefaultParameters {
  /** aircraft configuration used for analysis */
  var aircraftData: ShortRange.type = ShortRange
  /** default lower bound used for lambdas */
  var lambdaLowerBound = BigDecimal(-12.0)
  /** default upper bound used for lambdas */
  var lambdaUpperBound = BigDecimal(-2.0)
  /** Coefficient for cut selector in budget constraint. */
  var K1 = BigDecimal("500.0")
  /** Coefficient for cut selector in latency limitation constraint. */
  var K2 = BigDecimal("500.0")
  /** Latency limitation constant */
  var latencyLimitationBound = BigDecimal("-3.0")
  /** Residual risk limitation constant */
  var residualRiskLimitationBound = BigDecimal("-5.0")
  /** global upper bound for lambdas */
  var globalLambdaUpperBound = BigDecimal("100.0")
  /** global lower bound for lambdas */
  var globalLambdaLowerBound = BigDecimal("-100.0")
  /** if set to true, bounds will be generated for min variables */
  var genMinBounds = false
  /** Optimisation criterion */
  var criterion: BudgetCriterion = Combined2Criterion
}
