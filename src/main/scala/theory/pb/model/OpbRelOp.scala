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

package theory.pb.model

/**
 * Base class for relational operators used in pseudo-boolean constraints.
 * */
sealed trait OpbRelOp

/**
 * Less-than or equal.
 * */
case object OpbLe extends OpbRelOp {
  override def toString = "<="
}

/**
 * Greater-than or equal.
 * */
case object OpbGe extends OpbRelOp {
  override def toString = ">="
}

/**
 * Strictly-less-than.
 * */
case object OpbLt extends OpbRelOp {
  override def toString = "<"
}

/**
 * Strictly-greater-than.
 * */
case object OpbGt extends OpbRelOp {
  override def toString = ">"
}


