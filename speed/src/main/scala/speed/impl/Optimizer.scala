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

import scala.annotation.tailrec

trait Optimizer { self: SpeedImpl ⇒
  import c.universe._

  def optimize(chain: OperationChain): OperationChain = {
    @tailrec def optimizeTerminalOnce(chain: OperationChain): OperationChain =
      optimizeTerminal(chain.terminal) match {
        case GenerationChangingTerminal(cons, term) ⇒ optimizeTerminalOnce(OperationChain(cons(chain.generator), term))
        case term @ _                               ⇒ OperationChain(optimizeGen(chain.generator), term)
      }

    optimizeTerminalOnce(chain)
  }

  def optimizeGen(gen: Generator): Generator = gen match {
    case IndexedGenerator(array) ⇒
      val arrayVar = c.fresh(newTermName("array$"))
      val init = q"val $arrayVar = $array"

      optimizeGen(
        InitAddingGenerator(
          MappingGenerator(RangeGenerator(q"0", q"$arrayVar.length", q"1", q"false"), Closure("idx", q"$arrayVar(idx)", q"")),
          Seq(init)))
    case ReverseGenerator(outer)      ⇒ optimizeGen(reverseRoot(optimizeGen(outer)))
    case TakeGenerator(outer, number) ⇒ optimizeTake(number)(outer)
    case i: InnerGenerator            ⇒ i.transformOuter(optimizeGen)
    case _                            ⇒ gen
  }

  def optimizeTerminal(terminal: TerminalOperation): TerminalOperation = terminal match {
    case Sum(num) ⇒ FoldLeft(q"$num.zero", closure2(q"$num.plus(_, _)"))
    case Min(ord) ⇒ minOrMax("min", ord)
    case Max(ord) ⇒ minOrMax("max", ord)
    case Size     ⇒ FoldLeft(q"0", closure2(q"((num, _) => num + 1)"))
    case Count(f) ⇒ GenerationChangingTerminal(FilteringGenerator(_, f), Size)
    case _        ⇒ terminal
  }

  def minOrMax(op: TermName, ord: Tree): TerminalOperation = {
    val Ord = typeOf[Ordering[_]].typeSymbol.asClass
    val OrdT = Ord.typeParams(0).asType.toType
    val cand = OrdT.asSeenFrom(ord.tpe, Ord)

    Reduce(cand, closure2(q"$ord.$op(_, _)"))
  }

  def reverseRoot(outer: Generator): Generator = outer match {
    case i: InnerGenerator                       ⇒ i.transformOuter(reverseRoot)

    // for simple Ranges it's simple...
    case RangeGenerator(start, end, q"1", incl)  ⇒ RangeGenerator(q"if ($incl) $end else $end - 1", start, q"-1", q"true")
    case RangeGenerator(start, end, q"-1", incl) ⇒ RangeGenerator(q"if ($incl) $end else $end + 1", start, q"1", q"true")

    // however when step != +/- 1 we'd need much of the range logic, so we stay from it right now and
    // fall back on the range runtime implementation
    // TODO: one easier possibility to fix it would be to make the RangeGenerator / RangeGeneration itself
    // able to deal with reversion
    case RangeGenerator(start, end, by, incl) ⇒
      range(q"""
        (if ($incl) $start to $end by $by
        else $start until $end by $by).reverse
      """)

    case ListGenerator(l, tpe) ⇒ ListGenerator(q"$l.reverse", tpe)
  }

  def optimizeTake(number: Tree)(outer: Generator): Generator = optimizeGen(outer) match {
    // we optimize only the common cases (also used for array access)
    case RangeGenerator(start, end, q"1", q"false")  ⇒ RangeGenerator(start, q"math.min($start + $number, $end)", q"1", q"false")
    case RangeGenerator(start, end, q"-1", q"false") ⇒ RangeGenerator(start, q"math.max($start - $number, $end)", q"-1", q"false")

    case i: InitAddingGenerator                      ⇒ i.transformOuter(optimizeTake(number))
    case m: MappingGenerator if m.f.isPure           ⇒ m.transformOuter(optimizeTake(number))

    case opt @ _                                     ⇒ TakeGenerator(opt, number)
  }
}