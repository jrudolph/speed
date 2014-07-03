import scala.collection.immutable
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import speed.impl.MacroEntry
import scala.reflect.internal.annotations.compileTimeOnly

package object speed {
  implicit def RangesAreSpeedy(range: Range): Speedy[Int] = compileTimeOnly
  implicit def ArraysAreSpeedy[A](range: Array[A]): Speedy[A] = compileTimeOnly
  implicit def ListsAreSpeedy[A](list: List[A]): Speedy[A] = compileTimeOnly
  implicit def IndexedSeqsAreSpeedy[A](seq: immutable.IndexedSeq[A]): Speedy[A] = compileTimeOnly

  def compileTimeOnly =
    throw new IllegalStateException(
      "A `speedy` expression must end with a terminal operation. (Should have been checked at compile-time)")
}

package speed {
  trait Speedy[A] {
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    def speedy: OptimizedCollection[A]
  }

  trait NonTerminalOps[A] {
    def flatMap[B](f: A ⇒ OptimizedCollection[B]): OptimizedCollection[B]
    def map[B](f: A ⇒ B): OptimizedCollection[B]
    def filter(p: A ⇒ Boolean): OptimizedCollection[A]
    def withFilter(p: A ⇒ Boolean): OptimizedCollection[A]
    def reverse: OptimizedCollection[A]
    def take(f: Int): OptimizedCollection[A]
  }
  trait TerminalOps[A] {
    def foldLeft[B](init: B)(f: (B, A) ⇒ B): B = macro MacroEntry.entryFoldLeft[A, B]
    def foreach[T](f: A ⇒ T): Unit = macro MacroEntry.entryP1[Unit]
    def reduce[A1 >: A](f: (A1, A1) ⇒ A1): A1 = macro MacroEntry.entryP1[A1]
    def sum[B >: A](implicit i: Numeric[B]): B = macro MacroEntry.entryImplicitP1[Numeric[B], B]
    def min[B >: A](implicit i: Ordering[B]): A = macro MacroEntry.entryImplicitP1[Ordering[B], A]
    def max[B >: A](implicit i: Ordering[B]): A = macro MacroEntry.entryImplicitP1[Ordering[B], A]
    def count(f: A ⇒ Boolean): Int = macro MacroEntry.entryP1[Int]
    def size: Int = macro MacroEntry.entryP0[Int]
    def mkString: String = macro MacroEntry.entryP0[String]
    def to[Coll[_]](implicit i: CanBuildFrom[Nothing, A, Coll[A]]): Coll[A] = macro MacroEntry.entryImplicitP0[CanBuildFrom[Nothing, A, Coll[A]], Coll[A]]
    def forall(f: A ⇒ Boolean): Boolean = macro MacroEntry.entryP1[Boolean]
  }

  trait OptimizedCollection[A] extends NonTerminalOps[A] with TerminalOps[A]

  final class impure extends scala.annotation.StaticAnnotation

  /** Provides implicit conversions for common types where possible */
  object auto extends LowLevelAuto {
    type impure = speed.impure

    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def booleanArrayOps(a: Array[Boolean]): OptimizedCollection[Boolean] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def byteArrayOps(a: Array[Byte]): OptimizedCollection[Byte] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def charArrayOps(a: Array[Char]): OptimizedCollection[Char] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def doubleArrayOps(a: Array[Double]): OptimizedCollection[Double] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def floatArrayOps(a: Array[Float]): OptimizedCollection[Float] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def intArrayOps(a: Array[Int]): OptimizedCollection[Int] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def longArrayOps(a: Array[Long]): OptimizedCollection[Long] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def refArrayOps[T <: AnyRef](a: Array[T]): OptimizedCollection[T] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def shortArrayOps(a: Array[Short]): OptimizedCollection[Short] = compileTimeOnly
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def unitArrayOps(a: Array[Unit]): OptimizedCollection[Unit] = compileTimeOnly

    trait OptimizedRange extends OptimizedCollection[Int] {
      def by(step: Int): OptimizedCollection[Int]
    }
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    implicit def intWrapper(from: Int): {
      def to(to: Int): OptimizedRange
      def until(until: Int): OptimizedRange
    } = compileTimeOnly
  }
  /** Provided to prevent ambiguities with Predef's LowLevelImplicits */
  trait LowLevelAuto {
    implicit def wrapBooleanArray(a: Array[Boolean]) = Predef.wrapBooleanArray(a)
    implicit def wrapByteArray(a: Array[Byte]) = Predef.wrapByteArray(a)
    implicit def wrapCharArray(a: Array[Char]) = Predef.wrapCharArray(a)
    implicit def wrapDoubleArray(a: Array[Double]) = Predef.wrapDoubleArray(a)
    implicit def wrapFloatArray(a: Array[Float]) = Predef.wrapFloatArray(a)
    implicit def wrapIntArray(a: Array[Int]) = Predef.wrapIntArray(a)
    implicit def wrapLongArray(a: Array[Long]) = Predef.wrapLongArray(a)
    implicit def wrapRefArray[T <: AnyRef](a: Array[T]) = Predef.wrapRefArray(a)
    implicit def wrapShortArray(a: Array[Short]) = Predef.wrapShortArray(a)
    implicit def wrapUnitArray(a: Array[Unit]) = Predef.wrapUnitArray(a)
  }
}
