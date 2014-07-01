package speed

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{ Gen, Arbitrary }
import Arbitrary.arbitrary
import speed.impl.Debug

case class Ref(n: Int) {
  def t(f: Int ⇒ Int): Ref = Ref(f(n))
  def t2(other: Ref)(f: (Int, Int) ⇒ Int): Ref = Ref(f(n, other.n))
}

class CheckTests extends Specification with ScalaCheck {
  trait NumericExtractor[T] {
    type Res
    def res: Res
  }
  implicit def collNumericExtractor[Coll[_], T](implicit num: Numeric[T]): NumericExtractor[Coll[T]] { type Res = Numeric[T] } =
    new NumericExtractor[Coll[T]] {
      type Res = Numeric[T]
      def res = num
    }
  implicit object RangeNumericExtractor extends NumericExtractor[Range] {
    type Res = Numeric[Int]
    def res = implicitly[Numeric[Int]]
  }
  def num[T](implicit ex: NumericExtractor[T]): ex.Res = ex.res

  implicit object RefIsNumeric extends Numeric[Ref] {
    def compare(x: Ref, y: Ref): Int = java.lang.Integer.compare(x.n, y.n)
    def toDouble(x: Ref): Double = x.n.toDouble
    def toFloat(x: Ref): Float = x.n.toFloat
    def toLong(x: Ref): Long = x.n.toLong
    def toInt(x: Ref): Int = x.n
    def fromInt(x: Int): Ref = Ref(x)
    def negate(x: Ref): Ref = x.t(-_)
    def times(x: Ref, y: Ref): Ref = x.t2(y)(_ * _)
    def minus(x: Ref, y: Ref): Ref = x.t2(y)(_ - _)
    def plus(x: Ref, y: Ref): Ref = x.t2(y)(_ + _)
  }
  implicit def refIsComparable: Ordering[Ref] = Ordering.by[Ref, Int](_.n)

  implicit def genRef: Gen[Ref] =
    for {
      i ← arbitrary[Int]
    } yield Ref(i)

  implicit val genRefArray = Arbitrary(Gen.containerOf[Array, Ref](genRef))
  implicit val genRange = Arbitrary[Range] {
    for {
      start ← Gen.choose(Int.MinValue / 4, Int.MaxValue / 4)
      size ← Gen.choose(0, 8)
      step ← Gen.oneOf(1, 2, 3, -1, -3) //Gen.oneOf(1, -1, 3, -3, 5, -5, 99, -99, 100, -100)
    } yield new Range.Inclusive(start, (start + size * step + 1), step) {
      override def toString(): String = s"Range(start = $start, end = $end, step = $step, toString = ${super.toString()})"
    }
  }

  //implicit val arbDouble = Arbitrary(Arbitrary.arbDouble.arbitrary suchThat (!java.lang.Double.isNaN(_)))

  GenerateTests.generateTests[Array[Int]]()
  GenerateTests.generateTests[Array[Long]]()
  //GenerateTests.generateTests[Array[Double]]()
  //GenerateTests.generateTests[Array[Float]]()
  GenerateTests.generateTests[Array[Ref]]()

  GenerateTests.generateTests[Range]()
  GenerateTests.generateTests[List[Int]]()
  GenerateTests.generateTests[List[Long]]()
}
