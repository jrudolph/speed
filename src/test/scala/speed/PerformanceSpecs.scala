package speed

import org.specs2.mutable.Specification
import ichi.bench.Thyme

object ThymeExtras {
  implicit class ComparisonExtra(comp: Thyme.Comparison) {
    // see https://github.com/Ichoran/thyme/blob/master/bench/Thyme.scala#L1325
    def significantlyDifferent = comp.foldChangeRuntime.pfitdiff < 0.05
    def factor = comp.foldChangeRuntime.rdiff.mean
  }
}

class PerformanceSpecs extends Specification {
  "Speedy" should {
    "make ranges as fast as while loops" in {
      "simple counting" in {
        beSimilarlyFast {
          var counter = 0
          for (i ← 1 to 1000) counter += i * i
          counter
        } {
          var counter = 0
          var i = 1
          while (i <= 1000) {
            counter += i * i
            i += 1
          }
          counter
        }
      }
      "nested counting" in {
        beSimilarlyFast {
          var counter = 0
          for {
            i ← 1 to 100
            j ← 1 to 100
          } counter += i * j
          counter
        } {
          var counter = 0
          var i = 1
          while (i <= 100) {
            var j = 1
            while (j <= 100) {
              counter += i * j
              j += 1
            }

            i += 1
          }
          counter
        }
      }
      "summing" in {
        beSimilarlyFast {
          (1 to 1000).sum
        } {
          var counter = 0
          var i = 1
          while (i <= 1000) {
            counter += i
            i += 1
          }
          counter
        }
      }
      "mapped summing" in {
        beSimilarlyFast {
          (1 to 1000).map(i ⇒ i * i).sum
        } {
          var counter = 0
          var i = 1
          while (i <= 1000) {
            counter += i * i
            i += 1
          }
          counter
        }
      }
    }
    "improve speed of Array.foreach" in {
      "summing up array elements" in {
        val array = Array.tabulate[Int](1000)(identity)
        beSimilarlyFast {
          var counter = 0
          for (x ← array) counter += x * x
          counter
        } {
          var counter = 0
          var i = 0
          while (i < array.length) {
            val x = array(i)
            counter += x * x
            i += 1
          }
          counter
          /*implicit val x = Predef.wrapIntArray _
          var counter = 0
          for (x ← array) counter += x
          counter*/
        }
      }
    }
  }

  import ThymeExtras._
  val th = ichi.bench.Thyme.warmed(verbose = print)
  def beSimilarlyFast[T](a: ⇒ T)(b: ⇒ T) = {
    val result = th.benchOffPair(targetTime = 1)(a)(b)._2
    !result.significantlyDifferent || result.factor > 0.99 must beTrue.or(failure("Wasn't matching as fast but " + result))
  }

}

