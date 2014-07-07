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

trait RangeGeneration { self: SpeedImpl ⇒
  import c.universe._

  def generateRangeNew(start: Expr[Int], end: Expr[Int], step: Expr[Int], isInclusive: Expr[Boolean], cancel: Cancel): ExprGen[Int] = {
    // a variable whose value decides which implementation to use
    //  1: count up and compare with `<` / `<=`
    // -1: count down and compare with `>` / `>=`
    //  0: safe version, if abs(step) != 1, to make sure not to overflow the bounds
    // we invoke that here eagerly and use our handy partial evaluation implementation
    // to figure out if we can decide now or not
    // if we can't figure it out now, we choose the variant which always works
    val decider =
      foldConstants(
        reify {
          step.splice match {
            case 0  ⇒ throw new IllegalArgumentException("step cannot be 0.")
            case 1  ⇒ if (isInclusive.splice && (end.splice.toLong + step.splice > Int.MaxValue)) 0 else 1
            case -1 ⇒ if (isInclusive.splice && (end.splice.toLong + step.splice < Int.MinValue)) 0 else -1
            case _ ⇒
              if (step.splice > 0)
                if (end.splice.toLong + step.splice > Int.MaxValue) 0 // overflow looming
                else 1
              else if (end.splice.toLong + step.splice < Int.MinValue) 0 // overflow looming
              else -1
          }
        }.tree) match {
          case l @ Literal(Constant(x))           ⇒ x
          case Block(_, l @ Literal(Constant(x))) ⇒ x
          case x @ _                              ⇒ 0
        }

    inner ⇒ decider match {
      case 1 ⇒ // count up
        reify {
          var cur = start.splice
          val endVal = end.splice
          val stepVal = step.splice

          while (((!isInclusive.splice && (cur < endVal)) ||
            (isInclusive.splice && (cur <= endVal)))
            && !cancel.shouldCancel.splice) {

            inner(cur.reified).splice
            cur += stepVal
          }
        }
      case -1 ⇒ // count down
        reify {
          var cur = start.splice
          val endVal = end.splice
          val stepVal = step.splice

          while (((!isInclusive.splice && (cur > endVal)) ||
            (isInclusive.splice && (cur >= endVal)))
            && !cancel.shouldCancel.splice) {

            inner(cur.reified).splice
            cur += stepVal
          }
        }
      case 0 ⇒
        reify {
          val startVal = start.splice
          val endVal = end.splice
          val stepVal = step.splice

          val terminalElement = {
            val gap = endVal.toLong - startVal.toLong
            val isExact = gap % stepVal == 0
            val hasStub = isInclusive.splice || !isExact
            val longLength = gap / stepVal + (if (hasStub) 1 else 0)
            val isEmpty =
              (startVal > endVal && stepVal > 0) ||
                (startVal < endVal && stepVal < 0) ||
                (startVal == endVal && !isInclusive.splice)
            val numRangeElements =
              if (isEmpty) 0
              else longLength
            (startVal.toLong + numRangeElements * stepVal).toInt
          }

          var cur = startVal
          while (cur != terminalElement && !cancel.shouldCancel.splice) {
            inner(cur.reified).splice
            cur += stepVal
          }
        }
    }
  }
}