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

import scala.reflect.macros.Context
import scala.annotation.unchecked.uncheckedStable

object GenerateTests {
  def generateTests[Coll](): Any = macro generateTestsMacro[Coll]

  def generateTestsMacro[Coll: c.WeakTypeTag](c: Context)(): c.Expr[Any] = c.Expr[Any] {
    import c.universe._

    val coll = weakTypeOf[Coll]
    val typTree = TypeTree(coll)
    val name = c.literal(coll.toString)

    q"""
    $name should {
      val numeric = num[$typTree]
      import Numeric.Implicits._
      import Ordering.Implicits._

      "sum" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.sum === coll.sum
        }
      }
      "min" in {
        prop { (coll: $typTree) ⇒
          coll.nonEmpty ==> (coll.speedy.min === coll.min)
        }
      }
      "max" in {
        prop { (coll: $typTree) ⇒
          coll.nonEmpty ==> (coll.speedy.max === coll.max)
        }
      }
      "mapped sum" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.map(x => x * x).sum === coll.map(x => x * x).sum
        }
      }
      "filtered sum" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.filter(_ > numeric.zero).sum === coll.filter(_ > numeric.zero).sum
        }
      }
      "foldLeft" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.foldLeft(numeric.one)(_ * _) === coll.foldLeft(numeric.one)(_ * _)
        }
      }
      "reduce" in {
        prop { (coll: $typTree) ⇒
          coll.nonEmpty ==>
            (coll.speedy.reduce(_ * _) === coll.reduce(_ * _))
        }
      }
      "size" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.size === coll.size
        }
      }
      "count" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.count(_ > numeric.zero) === coll.count(_ > numeric.zero)
        }
      }
      "mkString" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.mkString === coll.mkString
        }
      }
      "to[Vector]" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.to[Vector] === coll.to[Vector]
        }
      }
      "to[List]" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.to[List] === coll.to[List]
        }
      }
      "to[Array]" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.to[Array] === coll.to[Array]
        }
      }
      "forall" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.forall(_ > numeric.zero) === coll.forall(_ > numeric.zero)
        }
      }
      "exists" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.exists(_ > numeric.zero) === coll.exists(_ > numeric.zero)
        }
      }
      "reverse" in {
        prop { (coll: $typTree) ⇒
          coll.speedy.reverse.mkString === coll.reverse.mkString
        }
      }
      "take" in {
        "simple take" in prop { (coll: $typTree) ⇒
          coll.speedy.take(2).sum === coll.take(2).sum
        }
        "optimize take through map" in prop { (coll: $typTree) ⇒
          coll.speedy.map(x => x * x).take(2).sum === coll.map(x => x * x).take(2).sum
        }
        "don't optimize take through impure map" in prop { (coll: $typTree) ⇒
          var counter = 0
          var counter2 = 0
          coll.speedy.map({ x ⇒ counter += 1; x + numeric.one }: @impure).take(2).sum ===
            coll.map { x ⇒ counter2 += 1; x + numeric.one }.take(2).sum
          counter === counter2
        }
        "don't optimize take through filter" in prop { (coll: $typTree) ⇒
          coll.speedy.filter(_ > numeric.zero).map(x => x * x).take(2).sum ===
            coll.filter(_ > numeric.zero).map(x => x * x).take(2).sum
        }
      }
    }
    """
  }
}
