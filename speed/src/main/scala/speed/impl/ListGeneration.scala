/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 Johannes Rudolph
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package speed.impl

trait ListGeneration { self: SpeedImpl ⇒
  import c.universe._

  def generateList[T](l: Expr[List[T]], listTpe: Type, cancelVar: Cancel): ExprGen[T] =
    { inner ⇒
      implicit val t = listElementTypeTag[T](listTpe)

      reify {
        var current = l.splice
        while (current.isInstanceOf[_root_.scala.collection.immutable.::[_]] && !cancelVar.shouldCancel.splice) {
          val curCons = current.asInstanceOf[_root_.scala.collection.immutable.::[T]]
          inner(reifyInner(curCons.head)).splice
          current = curCons.tail
        }
      }
    }

  def listElementTypeTag[T](listTpe: Type): WeakTypeTag[T] = {
    val tpe = TypeTree(listTpe)
    val List = typeOf[List[_]].typeSymbol.asClass
    val ListA = List.typeParams(0).asType.toType
    val innerTpe = ListA.asSeenFrom(listTpe, List)
    c.WeakTypeTag[T](innerTpe)
  }
}
