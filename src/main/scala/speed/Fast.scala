package object speed {

  import scala.language.experimental.macros

  implicit class intWrapper(from: Int) {
    def to(to: Int): FastRange = ???
  }

  case class FastRange(from: Int, to: Int) {
    def foreach[T](f: Int ⇒ T): Unit = macro FastMacros.foreachImpl[T]
  }
}
