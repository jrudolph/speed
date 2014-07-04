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

trait Generation extends RangeGeneration with ListGeneration { self: SpeedImpl ⇒
  import c.universe._
  case class GeneratorSetup(inits: Seq[Tree], body: Tree) {
    def prependInits(inits: Seq[Tree]): GeneratorSetup = copy(inits = inits ++ this.inits)
  }
  object GeneratorSetup {
    implicit def treeToSetup(t: Tree): GeneratorSetup = GeneratorSetup(Nil, t)
  }
  case class TerminalOperationSetup(inits: Seq[Tree], inner: Tree, result: Tree)

  def generate(chain: OperationChain): Tree = {
    val cancelVar = c.fresh(newTermName("cancel$"))
    val varName = newTermName(c.fresh("value$"))

    val term = generateTerminal(chain.terminal, varName, cancelVar)
    //println(s"Term: $term")
    val GeneratorSetup(genInits, gen) = generateGen(chain.generator, varName, term.inner, cancelVar)
    //println(s"Gen: $gen")

    q"""
      var $cancelVar = false
      ..${genInits ++ term.inits}

      $gen

      ${term.result}
    """
  }

  def generateGen(gen: Generator, expectedValName: TermName, application: Tree, cancelVar: TermName): GeneratorSetup = gen match {
    case RangeGenerator(start, end, by, incl) ⇒ generateRange(start, end, by, incl, expectedValName, application, cancelVar)
    case ListGenerator(l, tpe)                ⇒ generateList(l, tpe, expectedValName, application, cancelVar)
    case MappingGenerator(outer, f) ⇒
      val tempName = c.fresh(newTermName("m$"))
      val body =
        q"""
            val $expectedValName = {
              val ${f.valName} = $tempName
              ${f.application}
            }
            $application
          """

      generateGen(outer, tempName, body, cancelVar)

    case FlatMappingGenerator(outer, innerValName, innerGenerator) ⇒
      val GeneratorSetup(inits, innerLoop) = generateGen(innerGenerator, expectedValName, application, cancelVar /* FIXME: is this correct? */ )
      generateGen(outer, innerValName, innerLoop, cancelVar).prependInits(inits)

    case FilteringGenerator(outer, f) ⇒
      val tempName = c.fresh(newTermName("m$"))
      val body =
        q"""
            if ({
              val ${f.valName} = $tempName
              ${f.application}
            }) {
              val $expectedValName = $tempName
              $application
            }
          """

      generateGen(outer, tempName, body, cancelVar)

    case InitAddingGenerator(outer, inits) ⇒ generateGen(outer, expectedValName, application, cancelVar).prependInits(inits)

    case TakeGenerator(outer, number) ⇒
      val counterVar = c.fresh(newTermName("counter$"))
      val init = q"var $counterVar = 0"
      val body = q"""
        if ($counterVar < $number) $application
        $counterVar += 1
      """

      // TODO: think about an early cancelling version
      // $cancelVar = $cancelVar || ($counterVar >= $number)

      generateGen(outer, expectedValName, body, cancelVar).prependInits(Seq(init))
  }
  def generateTerminal(terminal: TerminalOperation, valName: TermName, cancelVar: TermName): TerminalOperationSetup = terminal match {
    case Foreach(f) ⇒
      val body: Tree = q"""
      {
        val ${f.valName} = $valName
        ${f.application}
      }
      """

      TerminalOperationSetup(Seq(f.init), body, q"()")

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

    case MkString ⇒
      val sbVar = c.fresh(newTermName("sb$"))
      val init = q"val $sbVar = new java.lang.StringBuilder"
      val body = q"$sbVar.append($valName.toString)"
      val result = q"$sbVar.toString"

      TerminalOperationSetup(Seq(init), body, result)

    case To(cbf) ⇒
      val builderVar = c.fresh(newTermName("builder$"))
      val init = q"val $builderVar = $cbf()"
      val body = q"$builderVar += $valName"
      val result = q"$builderVar.result()"

      TerminalOperationSetup(Seq(init), body, result)

    case Forall(f) ⇒ forAllExistsGen(f, valName, cancelVar, true, result ⇒ q"!$result")
    case Exists(f) ⇒ forAllExistsGen(f, valName, cancelVar, false, result ⇒ q"$result")
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