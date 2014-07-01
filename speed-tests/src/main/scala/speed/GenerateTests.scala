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
        prop { (array: $typTree) ⇒
          array.speedy.sum === array.sum
        }
      }
      "min" in {
        prop { (array: $typTree) ⇒
          array.nonEmpty ==> (array.speedy.min === array.min)
        }
      }
      "max" in {
        prop { (array: $typTree) ⇒
          array.nonEmpty ==> (array.speedy.max === array.max)
        }
      }
      "mapped sum" in {
        prop { (array: $typTree) ⇒
          array.speedy.map(x => x * x).sum === array.map(x => x * x).sum
        }
      }
      "filtered sum" in {
        prop { (array: $typTree) ⇒
          array.speedy.filter(_ > numeric.zero).sum === array.filter(_ > numeric.zero).sum
        }
      }
      "foldLeft" in {
        prop { (array: $typTree) ⇒
          array.speedy.foldLeft(numeric.one)(_ * _) === array.foldLeft(numeric.one)(_ * _)
        }
      }
      "reduce" in {
        prop { (array: $typTree) ⇒
          array.nonEmpty ==>
            (array.speedy.reduce(_ * _) === array.reduce(_ * _))
        }
      }
    }
    """
  }
}
