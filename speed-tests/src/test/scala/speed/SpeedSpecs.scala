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

package speed

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed

import speed.impl.Debug.rangeForeach

class SpeedSpecs extends Specification with PendingUntilFixed {
  "Speed macros" should {
    "optimize Range operations" in {
      "provide Range.foreach with static bounds" in {
        "simple cases" in {
          val (a, b) = rangeForeach(0 to 15)
          a === b
        }
        "counting down" in {
          val (a, b) = rangeForeach(15 to 0 by -1)
          a === b
        }
        "with positive step" in {
          val (a, b) = rangeForeach(0 to 15 by 2)
          a === b
        }
        "with negative step" in {
          val (a, b) = rangeForeach(15 to 0 by -2)
          a === b
        }
        "from MinValue to MaxValue" in {
          val (a, b) = rangeForeach(Int.MinValue to Int.MaxValue by Int.MaxValue)
          a === b
        }
        "from MaxValue to MinValue" in {
          val (a, b) = rangeForeach(Int.MaxValue to Int.MinValue by -Int.MaxValue)
          a === b
        }
        "with positive step near MaxValue" in {
          val (a, b) = rangeForeach((Int.MaxValue - 2) to Int.MaxValue by 5)
          a === b
        }
        "with step == 1 near MaxValue" in {
          val (a, b) = rangeForeach((Int.MaxValue - 2) to Int.MaxValue)
          a === b
        }
        "with positive step near not yet near enough MaxValue" in {
          val (a, b) = rangeForeach((Int.MaxValue - 10) to (Int.MaxValue - 5) by 5)
          a === b
        }
        "with negative step near MinValue" in {
          val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by -5)
          a === b
        }
        "with step == -1 near MinValue" in {
          val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by -1)
          a === b
        }
        "with negative step not yet near enough MinValue" in {
          val (a, b) = rangeForeach((Int.MinValue + 10) to (Int.MinValue + 5) by -5)
          a === b
        }
      }
      "provide Range.foreach with dynamic bounds" in {
        val start0 = 0
        "simple cases" in {
          val (a, b) = rangeForeach(start0 to 15)
          a === b
        }
        "counting down" in {
          val (a, b) = rangeForeach(15 to start0 by -1)
          a === b
        }
        "with positive step" in {
          val (a, b) = rangeForeach(start0 to 15 by 2)
          a === b
        }
        "with negative step" in {
          val (a, b) = rangeForeach(15 to start0 by -2)
          a === b
        }
        val min = Int.MinValue
        val max = Int.MaxValue
        "from MinValue to MaxValue" in {
          val (a, b) = rangeForeach(min to Int.MaxValue by Int.MaxValue)
          a === b
        }
        "from MaxValue to MinValue" in {
          val (a, b) = rangeForeach(Int.MaxValue to min by -Int.MaxValue)
          a === b
        }

        "with positive step near MaxValue" in {
          val (a, b) = rangeForeach((max - 2) to Int.MaxValue by 5)
          a === b
        }
        "with step == 1 near MaxValue" in {
          val (a, b) = rangeForeach((max - 2) to Int.MaxValue)
          a === b
        }
        "with negative step near MinValue" in {
          val (a, b) = rangeForeach((min + 2) to Int.MinValue by -5)
          a === b
        }
        "with step == -1 near MinValue" in {
          val (a, b) = rangeForeach((min + 2) to Int.MinValue by -1)
          a === b
        }
      }

      "provide Range.foreach with dynamic step" in {
        val one = 1
        val minus1 = -1
        val two = 2
        val minus2 = -2
        val five = 5
        val minus5 = -5
        val min = Int.MinValue
        val max = Int.MaxValue
        "simple cases" in {
          val (a, b) = rangeForeach(0 to 15 by one)
          a === b
        }
        "counting down" in {
          val (a, b) = rangeForeach(15 to 0 by minus1)
          a === b
        }
        "with positive step" in {
          val (a, b) = rangeForeach(0 to 15 by two)
          a === b
        }
        "with negative step" in {
          val (a, b) = rangeForeach(15 to 0 by minus2)
          a === b
        }
        "from MinValue to MaxValue" in {
          val (a, b) = rangeForeach(Int.MinValue to Int.MaxValue by max)
          a === b
        }
        "from MaxValue to MinValue" in {
          val (a, b) = rangeForeach(Int.MaxValue to Int.MinValue by -max)
          a === b
        }

        "with positive step near MaxValue" in {
          val (a, b) = rangeForeach((Int.MaxValue - 2) to Int.MaxValue by five)
          a === b
        }
        "with step == 1 near MaxValue" in {
          val (a, b) = rangeForeach((Int.MaxValue - 2) to Int.MaxValue by one)
          a === b
        }
        "with negative step near MinValue" in {
          val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by minus5)
          a === b
        }
        "with step == -1 near MinValue" in {
          val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by minus1)
          a === b
        }
      }

      "provide foldLeft + other foldLeft derived ops" in {
        "foldLeft" in {
          (1 to 1000).speedy.foldLeft(0)(_ + _) === (((1 to 1000): Range).sum)
        }
        "foldLeft for mapped ranges" in {
          (1 to 1000).speedy.map(i ⇒ i * i).foldLeft(0)(_ + _) === (((1 to 1000): Range).map(i ⇒ i * i).sum)
        }
        "sum" in {
          (1 to 1000).speedy.sum === (((1 to 1000): Range).sum)
        }
        "sum for mapped ranges" in {
          (1 to 1000).speedy.map(i ⇒ i * i).sum === (((1 to 1000): Range).map(i ⇒ i * i).sum)
        }
        "reduce for mapped ranges" in {
          // FIXME: make this a static error
          (1000 to 1).speedy.reduce(_ + _) must throwA[UnsupportedOperationException]
          (1 to 1000).speedy.map(1+).reduce(_ + _) === (2 to 1001).sum
        }
        "double map" in {
          (1 to 1000).speedy.map(i ⇒ i).map(i ⇒ i * i).sum === (((1 to 1000): Range).map(i ⇒ i * i).sum)
        }
        "flatMap" in {
          val reference =
            (for {
              i ← (1 to 100): Range
              j ← (1 to 100): Range
            } yield i * j).sum
          val value =
            (for {
              i ← 1 to 100 speedy;
              j ← 1 to 100 speedy
            } yield i * j).sum

          value === reference
        }
        "don't constant fold into applications" in {
          (1 to 1000).speedy.map { i ⇒
            var j = 1
            def set(): Boolean = { j = i; true }
            // our peephole optimization is too aggressive to be used generally
            val result = set() || true

            j
          }.sum === (1 to 1000).sum
        }
      }
      "provide filter" in {
        "simple cases" in {
          (1 to 1000).speedy.filter(_ % 2 == 0).sum === ((1 to 1000): Range).filter(_ % 2 == 0).sum
        }
        "mapped" in {
          (1 to 1000).speedy.map(_.toString).filter(_.length == 2).reduce(_ + _) ===
            ((1 to 1000): Range).map(_.toString).filter(_.length == 2).reduce(_ + _)
        }
      }
      "provide withFilter" in {
        "simple" in {
          {
            for {
              x ← (0 to 1000).speedy if x % 2 == 0
            } yield x * x
          }.sum === {
            for {
              x ← (0 to 1000) if x % 2 == 0
            } yield x * x
          }.sum
        }
      }
      "provide min" in {
        (1000 to 1 by -1).speedy.min === 1
      }
      "provide max" in {
        (1000 to 1 by -1).speedy.max === 1000
      }
      "provide size" in {
        (5 to 1000).speedy.size === (1000 - 5 + 1)
      }
      "provide to[Vector]" in {
        (5 to 1000).speedy.to[Vector] === (5 to 1000).to[Vector]
      }
      "exit early in forall" in {
        (0 to Int.MaxValue).speedy.forall(_ < 0) === false
      }

      "work with range variables" in {
        val r = (5 to 1000)

        r.speedy.map(x ⇒ x * x).sum === r.map(x ⇒ x * x).sum
      }
      "work with blocks returning a static range" in {
        {
          val d = 12
          def f(x: Int) = x * x
          val y = f(d)

          (y to 1000)
        }.speedy.map(x ⇒ x * x).sum === (144 to 1000).map(x ⇒ x * x).sum
      }
    }
    "optimize array operations" in {
      val array = Array.tabulate[Int](100)(identity)
      "foreach" in {
        var counter = 0
        for (x ← array.speedy) counter += x
        counter === (0 to 99).sum
      }
      "sum" in {
        array.speedy.sum === (0 to 99).sum
      }
      "mapped sum" in {
        array.speedy.map(_ + 1).sum === (1 to 100).sum
      }
      "mapped size" in {
        array.speedy.map(_ + 5).size === 100
      }
      "provide to[Vector]" in {
        array.speedy.to[Vector] === array.to[Vector]
      }
      "provide forall" in {
        array.speedy.filter(_ % 2 == 0).forall(_ % 3 == 0) === array.filter(_ % 2 == 0).forall(_ % 3 == 0)

        val empty = new Array[Int](0)
        empty.speedy.forall(_ % 3 == 0) === empty.forall(_ % 3 == 0)
      }
      "provide exists" in {
        array.speedy.filter(_ % 2 == 0).exists(_ % 3 == 0) === array.filter(_ % 2 == 0).exists(_ % 3 == 0)

        val empty = new Array[Int](0)
        empty.speedy.exists(_ % 3 == 0) === empty.exists(_ % 3 == 0)
      }
    }
    "optimize list operations" in {
      val list = List.tabulate[Int](100)(identity)
      "foreach" in {
        var counter = 0
        for (x ← list.speedy) counter += x
        counter === (0 to 99).sum
      }
      "sum" in {
        list.speedy.sum === (0 to 99).sum
      }
      "mapped sum" in {
        list.speedy.map(_ + 1).sum === (1 to 100).sum
      }
      "mapped size" in {
        list.speedy.map(_ + 5).size === 100
      }
      "provide to[Vector]" in {
        list.speedy.to[Vector] === list.to[Vector]
      }
    }
  }
}

