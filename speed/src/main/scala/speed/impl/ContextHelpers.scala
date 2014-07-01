package speed.impl

trait ContextHelpers { self: WithContext ⇒
  import c.universe._

  lazy val IntTag = c.weakTypeOf[Int]
  lazy val LongTag = c.weakTypeOf[Long]
  lazy val FloatTag = c.weakTypeOf[Float]
  lazy val DoubleTag = c.weakTypeOf[Double]
  lazy val ShortTag = c.weakTypeOf[Short]
  lazy val ByteTag = c.weakTypeOf[Byte]
  lazy val BooleanTag = c.weakTypeOf[Boolean]
  lazy val CharTag = c.weakTypeOf[Char]
  def neutralElement[T: c.WeakTypeTag]: Tree = neutralElement(c.universe.weakTypeOf[T])
  def neutralElement(tpe: Type): Tree =
    lit(tpe match {
      case IntTag     ⇒ 0
      case LongTag    ⇒ 0L
      case FloatTag   ⇒ 0f
      case DoubleTag  ⇒ 0d
      case ShortTag   ⇒ 0.toShort
      case ByteTag    ⇒ 0.toByte
      case BooleanTag ⇒ false
      case CharTag    ⇒ 0.toChar
      case _          ⇒ null
    })
  def lit(v: Any) = Literal(Constant(v))
}
