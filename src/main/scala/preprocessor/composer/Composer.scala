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

trait Composer[-Pre] {
    type Result
  def apply(pre:Pre) : Result
}

object Composer {

  type Aux[Pre,O] = Composer[Pre]{
    type Result = O
  }

  trait ComposerOps {

    def from[U](x: U): Derived[U] = x

    def from[U, V](x: U, y: V): Derived[(U, V)] = (x, y)

    def from[U, V, W](x: U, y: V, z: W): Derived[(U, V, W)] = (x, y, z)

    implicit class Derived[T](self: T) {
      def derive[U](implicit c: Composer.Aux[T, U]): U = c(self)
    }
  }
}



