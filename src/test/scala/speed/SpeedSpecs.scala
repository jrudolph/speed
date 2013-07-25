package speed

import org.specs2.mutable.Specification

class SpeedSpecs extends Specification {
  "Speed macros" should {
    "provide foreach with static bounds" in {
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
      "with positive step near not yet near enough MaxValue" in {
        val (a, b) = rangeForeach((Int.MaxValue - 10) to (Int.MaxValue - 5) by 5)
        a === b
      }
      "with negative step near MinValue" in {
        val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by -5)
        a === b
      }
      "with negative step not yet near enough MinValue" in {
        val (a, b) = rangeForeach((Int.MinValue + 10) to (Int.MinValue + 5) by -5)
        a === b
      }
    }
    "provide foreach with dynamic bounds" in {
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
      "with negative step near MinValue" in {
        val (a, b) = rangeForeach((min + 2) to Int.MinValue by -5)
        a === b
      }
    }

    "provide foreach with dynamic step" in {
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
      "with negative step near MinValue" in {
        val (a, b) = rangeForeach((Int.MinValue + 2) to Int.MinValue by minus5)
        a === b
      }
    }

    "provide foldLeft for mapped ranges" in {
      "simple" in {
        (1 to 1000).map(i ⇒ i * i).foldLeft(0)(_ + _) === (((1 to 1000): Range).map(i ⇒ i * i).sum)
      }
      "sum" in {
        (1 to 1000).map(i ⇒ i * i).sum === (((1 to 1000): Range).map(i ⇒ i * i).sum)
      }
      "double map" in {
        (1 to 1000).map(j ⇒ j).map(i ⇒ i * i).sum === (((1 to 1000): Range).map(i ⇒ i * i).sum)
      }
      "flatMap" in {
        val reference =
          (for {
            i ← (1 to 100): Range
            j ← (1 to 100): Range
          } yield i * j).sum
        val value =
          (for {
            i ← 1 to 100
            j ← 1 to 100
          } yield i * j).sum

        value === reference
      }
    }
    "fast array access" in {
      //val array: Array[AnyRef] = Array.tabulate[AnyRef](100)(x ⇒ x: java.lang.Integer)
      //array.foreach(println)

      import speed._
      //val wrapIntArray = null
      val array2 = Array.tabulate[Int](100)(identity)
      array2.foreach(println)
    }
  }
}

