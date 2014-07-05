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

trait Generation extends RangeGeneration with ListGeneration with Reifier { self: SpeedImpl ⇒
  import c.universe._
  case class GeneratorSetup(inits: Seq[Tree], body: Tree) {
    def prependInits(inits: Seq[Tree]): GeneratorSetup = copy(inits = inits ++ this.inits)
  }
  object GeneratorSetup {
    implicit def treeToSetup(t: Tree): GeneratorSetup = GeneratorSetup(Nil, t)
  }
  case class TerminalOperationSetup(inits: Seq[Tree], inner: Tree, result: Tree)

  case class Cancel(cancelVar: TermName) {
    val shouldCancel: Expr[Boolean] = Expr[Boolean](Ident(cancelVar))
    def cancel(shouldCancel: Expr[Boolean]): Expr[Unit] = Expr[Unit](q"$cancelVar = ${shouldCancel.tree}")
  }

  def generate(chain: OperationChain): Tree = {
    val cancel = Cancel(c.fresh(newTermName("cancel$")))
    val varName = newTermName(c.fresh("value$"))

    val generator = generateGenNew(cancel)(chain.generator)
    val terminal = generateTerminalNew(chain.terminal, cancel, generator)

    //val term = generateTerminal(chain.terminal, varName, cancelVar)
    //println(s"Term: $term")
    //val GeneratorSetup(genInits, gen) = generateGen(chain.generator, varName, term.inner, cancelVar)
    //println(s"Gen: $gen")

    q"""
      var ${cancel.cancelVar} = false

      ${terminal.tree}
    """
  }

  type ExprGen[T] = (Expr[T] ⇒ Expr[Unit]) ⇒ Expr[Unit]
  type ExprFunc[T, U] = Expr[T] ⇒ Expr[U]

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
  def generateGenNew[T](cancelVar: Cancel)(gen: Generator): ExprGen[T] = gen match {
    case MappingGenerator(outer, f)   ⇒ genMap(generateGenNew(cancelVar)(outer), closureApp(f))
    case FilteringGenerator(outer, f) ⇒ genFilter(generateGenNew(cancelVar)(outer), closureApp(f))
    case ListGenerator(l, tpe)        ⇒ generateList(Expr(l), tpe, cancelVar)

    case _ ⇒
      //gen: Generator, expectedValName: TermName, application: Tree, cancelVar: TermName
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

  def generateTerminalNew[T, U](terminal: TerminalOperation /* [T, U] */ , cancelVar: Cancel, generator: ExprGen[T]): Expr[U] = ((terminal match {
    case MkString   ⇒ genMkString(generator)
    case Foreach(f) ⇒ generator(closureApp(f))
    case _ ⇒
      val x = c.fresh(newTermName("x$"))
      val TerminalOperationSetup(inits, termBody, result) = generateTerminal(terminal, x, cancelVar)
      val body = generator { v ⇒
        Expr(q"""
          {
            val $x = ${v.tree}
            $termBody
          }
        """)
      }

      Expr(q"""
      ..$inits

      ${body.tree}

      $result
      """)
  }): Expr[_]).asInstanceOf[Expr[U]]

  def genMkString(generator: ExprGen[Any]): Expr[String] =
    reify {
      val builder = new java.lang.StringBuilder

      generator { value ⇒
        reifyInner(builder.append(value.splice))
      }.splice

      builder.toString
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
    case RangeGenerator(start, end, by, incl) ⇒ generateRange(start, end, by, incl, expectedValName, application, cancelVar.cancelVar)

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
  def generateTerminal(terminal: TerminalOperation, valName: TermName, cancelVar: Cancel): TerminalOperationSetup = terminal match {
    case FoldLeft(init, f) ⇒
      val accVar = c.fresh(newTermName("acc$"))
      val inits = Seq(q"var $accVar = ${init}", f.init)

      val body =
        q"""
          $accVar = {
            val ${f.valName1} = $accVar
            val ${f.valName2} = $valName
            ${f.application}
          }
        """

      TerminalOperationSetup(inits, body, q"$accVar")

    case Reduce(tpe, f) ⇒
      val accVar = c.fresh(newTermName("acc$"))
      val emptyVar = c.fresh(newTermName("empty$"))
      def b(b: Boolean) = Literal(Constant(b))
      val a1Type = tpe
      val neutralA1 = neutralElement(tpe)
      val inits = Seq(
        q"var $accVar: $a1Type = $neutralA1",
        q"var $emptyVar = ${b(true)}",
        f.init)
      val body =
        q"""
          if ($emptyVar) {
            $emptyVar = ${b(false)}
            $accVar = $valName
          } else
            $accVar = {
              val ${f.valName1} = $accVar
              val ${f.valName2} = $valName
              ${f.application}
            }
        """

      val result =
        q"""
              if ($emptyVar) throw new UnsupportedOperationException("Can't reduce empty range")
              else $accVar
            """

      TerminalOperationSetup(inits, body, result)

    case To(cbf) ⇒
      val builderVar = c.fresh(newTermName("builder$"))
      val init = q"val $builderVar = $cbf()"
      val body = q"$builderVar += $valName"
      val result = q"$builderVar.result()"

      TerminalOperationSetup(Seq(init), body, result)

    case Forall(f) ⇒ forAllExistsGen(f, valName, cancelVar.cancelVar, true, result ⇒ q"!$result")
    case Exists(f) ⇒ forAllExistsGen(f, valName, cancelVar.cancelVar, false, result ⇒ q"$result")
  }

  def forAllExistsGen(f: Closure,
                      valName: TermName,
                      cancelVar: TermName,
                      defaultValue: Boolean,
                      shouldCancel: (TermName) ⇒ Tree): TerminalOperationSetup = {
    val resultVar = c.fresh(newTermName("result$"))
    val init = q"var $resultVar = $defaultValue"
    val body = q"""
        $resultVar = {
          val ${f.valName} = $valName
          ${f.application}
        }
        $cancelVar = $cancelVar || ${shouldCancel(resultVar)}
      """
    val result = q"$resultVar"

    TerminalOperationSetup(Seq(init), body, result)
  }
}