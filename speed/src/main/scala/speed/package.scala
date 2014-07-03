import scala.collection.immutable
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import speed.impl.MacroEntry
import scala.reflect.internal.annotations.compileTimeOnly

package object speed {
  implicit class RangesAreSpeedy(range: Range) extends Speedy[Int]
  implicit class ArraysAreSpeedy[A](range: Array[A]) extends Speedy[A]
  implicit class ListsAreSpeedy[A](list: List[A]) extends Speedy[A]
  implicit class IndexedSeqsAreSpeedy[A](seq: immutable.IndexedSeq[A]) extends Speedy[A]

  def compileTimeOnly =
    throw new IllegalStateException(
      "A `speedy` expression must end with a terminal operation. (Should have been checked at compile-time)")
}

package speed {
  trait Speedy[A] {
    @compileTimeOnly("A `speedy` expression must end with a terminal operation.")
    def speedy: OptimizedColl[A] = ???
  }

  trait OptimizedInnerOps[A] {
    def flatMap[B](func: A ⇒ OptimizedColl[B]): OptimizedColl[B] = compileTimeOnly
    def map[B](func: A ⇒ B): OptimizedColl[B] = compileTimeOnly
    def filter(p: A ⇒ Boolean): OptimizedColl[A] = compileTimeOnly
    def withFilter(p: A ⇒ Boolean): OptimizedColl[A] = compileTimeOnly
    def reverse: OptimizedColl[A] = compileTimeOnly
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

  trait OptimizedColl[A] extends OptimizedInnerOps[A] with TerminalOps[A]
}
