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

import scala.collection.generic.CanBuildFrom

trait TerminalGeneration { self: SpeedImpl ⇒
  import c.universe._

  def generateTerminal[T, U](terminal: TerminalOperation, cancelVar: Cancel, generator: ExprGen[T]): Expr[U] = ((terminal match {
    case MkString          ⇒ genMkString(generator)
    case FoldLeft(init, f) ⇒ genFoldLeft(generator, Expr(init), closure2App(f))
    case Foreach(f)        ⇒ generator(closureApp(f))
    case Reduce(tpe, f)    ⇒ genReduce(generator, closure2App(f), tpe)
    case To(cbf)           ⇒ generateToColl(generator, Expr(cbf))
    case Forall(f)         ⇒ generateForallExists(generator, cancelVar, closureApp(f), c.literal(true), v ⇒ reify(!v.splice))
    case Exists(f)         ⇒ generateForallExists(generator, cancelVar, closureApp(f), c.literal(false), v ⇒ v)
  }): Expr[_]).asInstanceOf[Expr[U]]

  def genMkString(generator: ExprGen[Any]): Expr[String] =
    reify {
      val builder = new java.lang.StringBuilder

      generator { value ⇒
        reifyInner(builder.append(value.splice))
      }.splice

      builder.toString
    }

  def genFoldLeft[T, U](generator: ExprGen[T], init: Expr[U], foldF: ExprFunc2[U, T, U]): Expr[U] =
    reify {
      var acc = init.splice

      generator { value ⇒
        reifyInner {
          acc = foldF(reifyInner(acc), value).splice
        }
      }.splice

      acc
    }

  def genReduce[T](generator: ExprGen[T], f: ExprFunc2[T, T, T], tpe: Type): Expr[T] = {
    implicit val tTag = c.WeakTypeTag[T](tpe)

    reify {
      var acc: T = neutralElementExpr[T].splice
      var empty = true

      generator { value ⇒
        reifyInner {
          if (empty) {
            empty = false
            acc = value.splice
          } else acc = f(reifyInner(acc), value).splice
        }
      }.splice

      if (empty) throw new UnsupportedOperationException("Can't reduce empty range")
      else acc
    }
  }

  def generateToColl[T, U](generator: ExprGen[T], cbf: Expr[CanBuildFrom[_, T, U]]): Expr[U] = {
    reify {
      val builder = cbf.splice()

      generator { value ⇒
        reifyInner {
          builder += value.splice
        }
      }.splice

      builder.result()
    }
  }

  def generateForallExists[T](generator: ExprGen[T], cancel: Cancel, f: ExprFunc[T, Boolean], defaultValue: Expr[Boolean], shouldCancel: Expr[Boolean] ⇒ Expr[Boolean]): Expr[Boolean] =
    reify {
      var result = defaultValue.splice

      generator { value ⇒
        reifyInner {
          result = f(value).splice
          val newCancelValue = cancel.shouldCancel.splice || shouldCancel(reifyInner(result)).splice

          cancel.cancel(reifyInner(newCancelValue)).splice
        }
      }.splice

      result
    }
}
