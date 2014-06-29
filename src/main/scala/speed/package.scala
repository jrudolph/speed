import scala.language.experimental.macros

package object speed {
  implicit class intWrapper(from: Int) {
    def to(to: Int): FastRange = ???
    def until(until: Int): FastRange = ???
  }

  implicit class refArrayOps[T <: AnyRef](val oa: Array[T]) {
    def foreach[U](f: T ⇒ U): Unit = macro SpeedMacros.arrayForeachImpl[T, U]
  }

  implicit class wrapIntArray(oa: Array[Int]) {
    def foreach[U](f: Int ⇒ U): Unit = macro SpeedMacros.arrayForeachImpl[Int, U]
  }
  val intArrayOps = null

  implicit def isNormalRange(f: FastRange): Range = macro SpeedMacros.normalRangeConv
  implicit def isNormalRange(f: FastSteppedRange): Range = macro SpeedMacros.normalRangeConv
  implicit def mappedRange[U](f: MappedRange[U]): Seq[U] = macro SpeedMacros.mappedRangeImpl[U]

  def rangeForeach(range: Any): (List[Int], List[Int]) = macro SpeedMacros.rangeForeachImpl

  def show[T](t: T): T = macro SpeedMacros.showTree[T]
}

package speed {
  trait FastRange extends FastSteppedRange {
    def by(step: Int): FastSteppedRange
  }

  trait MappedRange[A] {
    def flatMap[B](func: A ⇒ MappedRange[B]): MappedRange[B]
    def map[B](func: A ⇒ B): MappedRange[B]
    def foldLeft[B](init: B)(f: (B, A) ⇒ B): B = macro SpeedMacros.foldLeftImpl[A, B]

    def reduce[A1 >: A](op: (A1, A1) ⇒ A1): A1 = macro SpeedMacros.reduceImpl[A1]
    def sum[B >: A](implicit num: Numeric[B]): B = macro SpeedMacros.sumImpl[A, B]
    def filter(p: A ⇒ Boolean): MappedRange[A]

    // other interesting foldLeft based operators to implement:
    // exists, forall, count
    // others:
    // find
  }

  trait FastSteppedRange {
    final def foreach[T](f: Int ⇒ T): Unit = macro SpeedMacros.foreachImpl[T]

    def map[U](func: Int ⇒ U): MappedRange[U]
    def filter(p: Int ⇒ Boolean): this.type
    def flatMap[B](func: Int ⇒ MappedRange[B]): MappedRange[B]
    def reduce(op: (Int, Int) ⇒ Int): Int = macro SpeedMacros.reduceImpl[Int]
    def foldLeft[B](init: B)(f: (B, Int) ⇒ B): B = macro SpeedMacros.foldLeftImpl[Int, B]
    def sum(implicit num: Numeric[Int]): Int = macro SpeedMacros.sumImpl[Int, Int]

    // others:
    // reduceOption
  }
}
