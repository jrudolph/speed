package speed

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import speed.impl.Debug

class CheckTests extends Specification with ScalaCheck {
  "Array[Int]" should {
    "sum" in {
      prop { (array: Array[Int]) ⇒
        println(array)
        array.speedy.sum === array.sum
      }
    }
    "min" in {
      prop { (array: Array[Int]) ⇒
        println(array)
        array.speedy.min === array.min
      }
    }
    "max" in {
      prop { (array: Array[Int]) ⇒
        println(array)
        array.speedy.max === array.max
      }
    }
    "mapped sum" in {
      prop { (array: Array[Int]) ⇒
        array.speedy.map(_ * 15).sum === array.map(_ * 15).sum
      }
    }
    "filtered sum" in {
      prop { (array: Array[Int]) ⇒
        array.speedy.filter(_ % 3 == 0).sum === array.filter(_ % 3 == 0).sum
      }
    }
    "foldLeft" in {
      prop { (array: Array[Int]) ⇒
        array.speedy.foldLeft(1)(_ * _) === array.foldLeft(1)(_ * _)
      }
    }
    "reduce" in {
      prop { (array: Array[Int]) ⇒
        array.nonEmpty ==>
          (array.speedy.reduce(_ * _) === array.reduce(_ * _))
      }
    }
  }
}
