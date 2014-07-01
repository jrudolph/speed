package speed.impl

import scala.reflect.macros.Context

object Debug {
  def show[T](t: T): T = macro DebugMacros.showTree[T]
  def rangeForeach(range: Any): (List[Int], List[Int]) = macro DebugMacros.rangeForeachImpl
}

object DebugMacros {
  def rangeForeachImpl(c: Context)(range: c.Expr[Any]): c.Expr[(List[Int], List[Int])] = {
    import c.universe._

    val r = range.asInstanceOf[c.Expr[Range]]
    val rSpeedy = c.Expr[Range](q"$range.speedy")

    c.universe.reify {
      import speed._
      val buffer = new scala.collection.mutable.ListBuffer[Int]
      var count = 0
      val x = 12
      rSpeedy.splice.foreach { element ⇒
        val gap = x
        require(count < 1000, "Too many iterations needed")
        count += 1
        buffer += element
      }
      //println(c.literal(range.toString).splice, buffer.toList, r.splice.iterator.toList, buffer.toList + "/" + r.splice.iterator.toList)
      assert(buffer.toList == r.splice.iterator.toList, buffer.toList + "/" + r.splice.iterator.toList)
      (buffer.toList, r.splice.iterator.toList)
    }
  }

  def showTree[T](c: Context)(t: c.Expr[T]): c.Expr[T] = { println(s"Show '${c.universe.show(t)}'"); t }
}