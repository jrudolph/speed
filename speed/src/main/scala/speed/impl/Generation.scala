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

import net.virtualvoid.macros.tools.Reifier
import scala.collection.generic.CanBuildFrom

trait Generation extends RangeGeneration with ListGeneration with TerminalGeneration with Reifier { self: SpeedImpl ⇒
  import c.universe._

  case class Cancel(cancelVar: TermName) {
    val shouldCancel: Expr[Boolean] = Expr[Boolean](Ident(cancelVar))
    def cancel(shouldCancel: Expr[Boolean]): Expr[Unit] = Expr[Unit](q"$cancelVar = ${shouldCancel.tree}")
  }

  def generate(chain: OperationChain): Tree = {
    val cancel = Cancel(c.fresh(newTermName("cancel$")))

    val generator = generateGen(cancel)(chain.generator)
    val terminal = generateTerminal(chain.terminal, cancel, generator)

    q"""
      var ${cancel.cancelVar} = false

      ${terminal.tree}
    """
  }

  def generateTerminal[T, U](terminal: TerminalOperation, cancel: Cancel, generator: ExprGen[T]): Expr[U]

  type ExprGen[T] = (Expr[T] ⇒ Expr[Unit]) ⇒ Expr[Unit]
  type ExprFunc[T, U] = Expr[T] ⇒ Expr[U]
  type ExprFunc2[T1, T2, U] = (Expr[T1], Expr[T2]) ⇒ Expr[U]

  def closureApp[T, U](cl: Closure): ExprFunc[T, U] = {
    v ⇒
      Expr[U](
        q"""
      {
        val ${cl.valName} = ${v.tree}
        ${cl.application}
      }
      """)
  }

  def closure2App[T1, T2, U](cl: Closure2): ExprFunc2[T1, T2, U] = {
    (v1, v2) ⇒
      Expr[U](
        q"""
      {
        val ${cl.valName1} = ${v1.tree}
        val ${cl.valName2} = ${v2.tree}
        ${cl.application}
      }
      """)
  }
  def generateGen[T](cancel: Cancel)(gen: Generator): ExprGen[T] = gen match {
    case MappingGenerator(outer, f)                ⇒ genMap(generateGen(cancel)(outer), closureApp(f))
    case FilteringGenerator(outer, f)              ⇒ genFilter(generateGen(cancel)(outer), closureApp(f))
    case TakeGenerator(outer, number)              ⇒ genTake(generateGen(cancel)(outer), Expr(number))

    case ListGenerator(l, tpe)                     ⇒ generateList(Expr(l), tpe, cancel)
    case RangeGenerator(start, end, by, inclusive) ⇒ generateRangeNew(Expr(start), Expr(end), Expr(by), Expr(inclusive), cancel).asInstanceOf[ExprGen[T]]

    case FlatMappingGenerator(outer, valName, inner) ⇒ {
      val outerGen = generateGen[T](cancel)(outer)
      val innerGen = generateGen(cancel)(inner)

      innerst ⇒
        outerGen { oValue ⇒
          Expr(q"""
            val $valName = ${oValue.tree}
            ${
            innerGen { iValue ⇒
              innerst(iValue)
            }.tree
          }
          """)
        }
    }

    case InitAddingGenerator(outer, inits) ⇒
      val outerGen = generateGen(cancel)(outer)

      inner ⇒
        Expr(q"""
          ..$inits

          ${
          outerGen { oValue ⇒
            inner(oValue)
          }.tree
        }
        """)

  }
  def genMap[T, U](outerGen: ExprGen[T], f: Expr[T] ⇒ Expr[U]): ExprGen[U] =
    (inner: Expr[U] ⇒ Expr[Unit]) ⇒
      outerGen { tVal ⇒
        reify {
          val uVal = f(tVal).splice
          inner(uVal.reified).splice
        }
      }

  def genFilter[T, U](outerGen: ExprGen[T], f: Expr[T] ⇒ Expr[Boolean]): ExprGen[T] =
    (inner: Expr[T] ⇒ Expr[Unit]) ⇒
      outerGen { tVal ⇒
        reify {
          if (f(tVal).splice) inner(tVal).splice
        }
      }

  def genTake[T](outerGen: ExprGen[T], number: Expr[Int]): ExprGen[T] =
    inner ⇒
      reify {
        var counter = 0
        val numberVal = number.splice

        outerGen { value ⇒
          {
            val uVal = value.splice
            if (counter < numberVal) inner(uVal.reified).splice
            counter += 1
          }.reified
        }.splice
      }
}