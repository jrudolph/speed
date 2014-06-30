import scala.language.experimental.macros
import speed.impl.SpeedMacrosV2

package object speed {
  implicit class RangesAreSpeedy(range: Range) extends Speedy[Int]
  implicit class ArraysAreSpeedy[A](range: Array[A]) extends Speedy[A]

  def compileTimeOnly = ???
}

package speed {
  trait Speedy[A] {
    def speedy: OptimizedColl[A] = ???
  }

  trait OptimizedInnerOps[A] {
    def flatMap[B](func: A ⇒ OptimizedColl[B]): OptimizedColl[B] = compileTimeOnly
    def map[B](func: A ⇒ B): OptimizedColl[B] = compileTimeOnly
    def filter(p: A ⇒ Boolean): OptimizedColl[A] = compileTimeOnly
  }
  trait TerminalOps[A] {
    def foldLeft[B](init: B)(f: (B, A) ⇒ B): B = macro SpeedMacrosV2.entryFoldLeft[A, B]
    def foreach[T](f: A ⇒ T): Unit = macro SpeedMacrosV2.entryP1[Unit]
    def reduce[A1 >: A](f: (A1, A1) ⇒ A1): A1 = macro SpeedMacrosV2.entryP1[A1]
    def sum[B >: A](implicit i: Numeric[B]): B = macro SpeedMacrosV2.entryImplicitP1[Numeric[B], B]
    def min[B >: A](implicit cmp: Ordering[B]): A
    def max[B >: A](implicit cmp: Ordering[B]): A
    def size: Int = ???

    // other interesting foldLeft based operators to implement:
    // exists, forall, count
    // others:
    // find
  }

  trait OptimizedColl[A] extends OptimizedInnerOps[A] with TerminalOps[A]
}
