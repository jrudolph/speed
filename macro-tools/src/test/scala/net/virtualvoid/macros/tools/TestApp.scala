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

  def testMacro(c: Context)(i: c.Expr[Int]): c.Expr[String] = new Reifier[c.type] {
    val ctx: c.type = c

    def inner(exp: Expr[Int]): Expr[String] = reify((exp.splice * 2).toString)

    val expr =
      reify[String] {
        val x = 5 + i.splice
        inner(reifyInner(x)).splice
      }

    def res: c.Expr[String] = expr

    /*

    q"""
      val x = 0 // what about hygiene?
      ${inner(Expr(q"x"))}
    """

     */

  }.res
}
