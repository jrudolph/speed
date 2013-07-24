import scala.language.experimental.macros

package object speed {
  implicit class intWrapper(from: Int) {
    def to(to: Int): FastRange = ???
    def until(until: Int): FastRange = ???
  }

  implicit def isNormalRange(f: FastRange): Range = macro SpeedMacros.normalRangeConv
  implicit def isNormalRange(f: FastSteppedRange): Range = macro SpeedMacros.normalRangeConv
  implicit def mappedRange[U](f: MappedRange[U]): Seq[U] = macro SpeedMacros.mappedRangeImpl[U]

  def rangeForeach(range: Any): (List[Int], List[Int]) = macro SpeedMacros.rangeForeachImpl
}

package speed {
  trait FastRange extends FastSteppedRange {
    def by(step: Int): FastSteppedRange
  }

  trait MappedRange[A] {
    def flatMap[B](func: A ⇒ MappedRange[B]): MappedRange[B]
    def map[B](func: A ⇒ B): MappedRange[B]
    def foldLeft[B](init: B)(f: (B, A) ⇒ B): B = macro SpeedMacros.foldLeftImpl[A, B]

    def sum[B >: A](implicit num: Numeric[B]): B = macro SpeedMacros.sumImpl[A, B]
  }

  trait FastSteppedRange {
    final def foreach[T](f: Int ⇒ T): Unit = macro SpeedMacros.foreachImpl[T]

    def map[U](func: Int ⇒ U): MappedRange[U]
    def flatMap[B](func: Int ⇒ MappedRange[B]): MappedRange[B]
    //override def foreach[T](f: Int ⇒ T): Unit = macro SpeedMacros.foreachImpl[T]
    //def size: Int = macro SpeedMacros.sizeImpl
  }
}
