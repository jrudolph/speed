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
  case class GeneratorSetup(inits: Seq[Tree], body: Tree) {
    def prependInits(inits: Seq[Tree]): GeneratorSetup = copy(inits = inits ++ this.inits)
  }
  object GeneratorSetup {
    implicit def treeToSetup(t: Tree): GeneratorSetup = GeneratorSetup(Nil, t)
  }

  case class Cancel(cancelVar: TermName) {
    val shouldCancel: Expr[Boolean] = Expr[Boolean](Ident(cancelVar))
    def cancel(shouldCancel: Expr[Boolean]): Expr[Unit] = Expr[Unit](q"$cancelVar = ${shouldCancel.tree}")
  }

  def generate(chain: OperationChain): Tree = {
    val cancel = Cancel(c.fresh(newTermName("cancel$")))

    val generator = generateGenNew(cancel)(chain.generator)
    val terminal = generateTerminal(chain.terminal, cancel, generator)

    q"""
      var ${cancel.cancelVar} = false

      ${terminal.tree}
    """
  }

  def generateTerminal[T, U](terminal: TerminalOperation, cancelVar: Cancel, generator: ExprGen[T]): Expr[U]

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
  def generateGenNew[T](cancelVar: Cancel)(gen: Generator): ExprGen[T] = gen match {
    case MappingGenerator(outer, f)                ⇒ genMap(generateGenNew(cancelVar)(outer), closureApp(f))
    case FilteringGenerator(outer, f)              ⇒ genFilter(generateGenNew(cancelVar)(outer), closureApp(f))
    case ListGenerator(l, tpe)                     ⇒ generateList(Expr(l), tpe, cancelVar)
    case RangeGenerator(start, end, by, inclusive) ⇒ generateRangeNew(Expr(start), Expr(end), Expr(by), Expr(inclusive), cancelVar).asInstanceOf[ExprGen[T]]

    case _ ⇒
      inner ⇒ {
        val v = c.fresh(newTermName("v$"))
        val app = inner(Expr(Ident(v))).tree
        val GeneratorSetup(inits, body) = generateGenOld(gen, v, app, cancelVar)

        Expr(q"""
        ..$inits

        $body
        """)
      }
  }
  def genMap[T, U](outerGen: ExprGen[T], f: Expr[T] ⇒ Expr[U]): ExprGen[U] =
    (inner: Expr[U] ⇒ Expr[Unit]) ⇒
      outerGen { tVal ⇒
        val uVal = f(tVal)
        inner(uVal)
      }

  def genFilter[T, U](outerGen: ExprGen[T], f: Expr[T] ⇒ Expr[Boolean]): ExprGen[T] =
    (inner: Expr[T] ⇒ Expr[Unit]) ⇒
      outerGen { tVal ⇒
        reify {
          if (f(tVal).splice) inner(tVal).splice
        }
      }

  def generateGen(gen: Generator, expectedValName: TermName, application: Tree, cancel: Cancel): GeneratorSetup = {
    val genny = generateGenNew(cancel)(gen)

    val generate = genny(v ⇒ Expr(q""" {
      val $expectedValName = ${v.tree}
      $application
    }
    """))

    generate.tree
  }
  def generateGenOld(gen: Generator, expectedValName: TermName, application: Tree, cancelVar: Cancel): GeneratorSetup = gen match {
    case FlatMappingGenerator(outer, innerValName, innerGenerator) ⇒
      val GeneratorSetup(inits, innerLoop) = generateGen(innerGenerator, expectedValName, application, cancelVar /* FIXME: is this correct? */ )
      generateGen(outer, innerValName, innerLoop, cancelVar).prependInits(inits)
    case InitAddingGenerator(outer, inits) ⇒ generateGen(outer, expectedValName, application, cancelVar).prependInits(inits)

    case TakeGenerator(outer, number) ⇒
      val counterVar = c.fresh(newTermName("counter$"))
      val init = q"var $counterVar = 0"
      val body = q"""
        if ($counterVar < $number) $application
        $counterVar += 1
      """

      // TODO: think about an early cancelling version, this however wouldn't work together with
      //       impure maps etc
      // $cancelVar = $cancelVar || ($counterVar >= $number)

      generateGen(outer, expectedValName, body, cancelVar).prependInits(Seq(init))
  }
}