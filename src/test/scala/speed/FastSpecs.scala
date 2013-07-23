package speed

import org.specs2.mutable.Specification

class FastSpecs extends Specification {
  "Fast macros" should {
    "provide foreach" in {
      (1 to 12).foreach(println _)
      (0 to 6).foreach { i ⇒
        println()
        println(i + 1)
      }
      val f = (i: Int) ⇒ println(i + 2)
      (133 to 137).foreach(f)

      1 === 1
    }
  }
}
