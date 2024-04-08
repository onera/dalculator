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

package theory.pb.translator

import theory.Logic.{BoolExp, BoolValue, False, Ident, IntExp, True}
import theory.pb.model.LitSum
import theory.pb.solver.ModelEvaluator

import scala.collection.immutable.HashMap

class BackTranslatorFactory(boolCache: HashMap[BoolExp, Int], boolIdentCache: HashMap[Ident, Int], intCache: HashMap[IntExp, LitSum]) {

  def get(litStrings: List[String], filter: String => Boolean = { _ => true }) = new BackTranslator(litStrings, filter)

  class BackTranslator(litStrings: List[String], filter: String => Boolean) {
    private val evaluate = new ModelEvaluator(litStrings)
    val identValues = new scala.collection.mutable.HashMap[Ident, BoolValue]()
    for ((id, lit) <- boolIdentCache if filter(id.name)) {
      evaluate(lit) match {
        case Some(b) => identValues.update(id, if (b) True else False)
        case None => ()
      }
    }
  }
}
