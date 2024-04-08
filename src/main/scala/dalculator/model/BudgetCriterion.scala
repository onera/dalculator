
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
 * */

package dalculator.model

/**
 * This file contains the classes used to
 *  - parse and model minimal cut sets
 *  - parse and model user defined constraints
 */
case class BudgetCriterion(
                            /** Weight of the minLambdaLatent variable in the criterion */
                            minLambdaGlobalW: BigDecimal = 0.0,

                            /** Weight of the minLambdaLatent variable in the criterion */
                            minLambdaLatentW: BigDecimal = 0.0,

                            /** Weight of the minLambdaActive variable in the criterion */
                            minLambdaActiveW: BigDecimal = 0.0,

                            /** Weight of the minSumLambdaLatent variable in the criterion */
                            minSumLambdaLatentW: BigDecimal = 0.0,

                            /** Weight of the minSumLambdaActive variable in the criterion */
                            minSumLambdaActiveW: BigDecimal = 0.0,

                            /** Weight of the minItvLatent variable in the criterion */
                            minItvLatentW: BigDecimal = 0.0)

/** A predefined criterion */
object LatentMinCriterion extends BudgetCriterion(0.0, 1.0, 0.0, 0.0, 0.0, 0.0)

/** A predefined criterion */
object ActiveMinCriterion extends BudgetCriterion(0.0, 0.0, 1.0, 0.0, 0.0, 0.0)

/** A predefined criterion */
object LatentSumCriterion extends BudgetCriterion(0.0, 0.0, 0.0, 1.0, 0.0, 0.0)

/** A predefined criterion */
object ActiveSumCriterion extends BudgetCriterion(0.0, 0.0, 0.0, 0.0, 1.0, 0.0)

/** A predefined criterion */
object IntervalMinCriterion extends BudgetCriterion(0.0, 0.0, 0.0, 0.0, 0.0, 1.0)

/** A predefined criterion */
object Combined1Criterion extends BudgetCriterion(0.0, 0.3, 0.0, 0.3, 0.0, 0.3)

/** A predefined criterion */
object Combined2Criterion extends BudgetCriterion(0.0, 0.2, 0.2, 0.2, 0.2, 0.2)

/** A predefined criterion */
object Combined3Criterion extends BudgetCriterion(0.0, 0.2, 0.2, 0.2, 0.2, 0.0)

/** A predefined criterion */
object CombinedGlobalCriterion extends BudgetCriterion(0.25, 0.25, 0.25, 0.25, 0.25, 0.0)

/** Optimize the global minimal of all lambdas. */
object GlobalMinCriterion extends BudgetCriterion(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
