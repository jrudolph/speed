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

import scala.reflect.macros.Context

object MacroEntry {
  def entryP0[R](c: Context): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryP1[R](c: Context)(f: c.Expr[Any]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryImplicitP0[I, R](c: Context)(i: c.Expr[I]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryImplicitP1[I, R](c: Context)(i: c.Expr[I]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryFoldLeft[T, U](c: Context)(init: c.Expr[U])(f: c.Expr[(U, T) ⇒ U]): c.Expr[U] =
    new TransformingSpeedContext[c.type](c).run[U]
}

