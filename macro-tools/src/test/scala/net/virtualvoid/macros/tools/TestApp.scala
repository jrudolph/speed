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

package net.virtualvoid.macros.tools

import scala.reflect.macros.Context

object TestMacro {
  def test(i: Int): String = macro testMacro

  def show[T](t: T): T = macro showTree[T]
  def showTree[T](c: Context)(t: c.Expr[T]): c.Expr[T] = { println(s"Show '${c.universe.show(t)}'"); t }

  abstract class Impl[C <: Context](ctx: C) extends Reifier {
    val c: C = ctx
  }

  def testMacro(c: Context)(i: c.Expr[Int]): c.Expr[String] = new Impl[c.type](c) {
    def transform(i: Expr[Int], j: Int): Expr[Int] = reifyShow {
      val x = i.splice

      (if (j > 0) (transform(x.reified, j - 1).splice + 23).reified else (x + 42).reified).splice
    }
    def res: c.Expr[String] = {
      val res = reifyShow {
        var x = 42

        transform(x.reified, 10).splice.toString
        //(x + (38 * i.splice).reified.splice).reified.splice.toString
        //transform(x.reified).splice(38).toString
      }
      println(c.universe.show(res.tree))
      res
    }
  }.res
}
