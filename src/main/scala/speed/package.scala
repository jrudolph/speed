import scala.language.experimental.macros

package object speed {
  implicit class intWrapper(from: Int) {
    def to(to: Int): FastRange = ???
    def until(until: Int): FastRange = ???
  }

  implicit def isNormalRange(f: FastRange): Range = macro SpeedMacros.normalRangeConv
  implicit def isNormalRange(f: FastSteppedRange): Range = macro SpeedMacros.normalRangeConv

  def rangeForeach(range: Any): (List[Int], List[Int]) = macro SpeedMacros.rangeForeachImpl
}

package speed {
  trait FastRange {
    final def foreach[T](f: Int ⇒ T): Unit = macro SpeedMacros.foreachImpl[T]

    def by(step: Int): FastSteppedRange
  }
  trait FastSteppedRange extends FastRange {
    //override def foreach[T](f: Int ⇒ T): Unit = macro SpeedMacros.foreachImpl[T]
    //def size: Int = macro SpeedMacros.sizeImpl
  }
}
